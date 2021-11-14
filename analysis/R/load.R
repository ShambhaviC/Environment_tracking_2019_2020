rm(list = ls())
require(tidyverse)
require(scales)
require(lubridate)
require(stringr)

folder <- "analysis/data/"

step_conv <- 0.324 # pump step conversion factor
##########################################
# Data from Experiment 1 - Subjective mean
##########################################
#-----------------------------------------
# Preparing the complete table of raw data
#-----------------------------------------
# Load the master table
daypathlist <- read.csv2(
  file = paste0(folder, "exp1_subjective_mean/meta_data/MasterTableSubjectiveMean.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# Load the conditions table
Conditions <- read.csv2(
  file = paste0(folder, "exp1_subjective_mean/meta_data/ConditionsSubjectiveMean.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# setting the colnames

mastercolnames <- c(
  "DateTime", "IdRFID", "IdLabel",
  "unitLabel", "eventDuration", "sense1duration",
  "sense1Events", "senseRFIDrecords", "reinforce1value",
  "reinforce1Total", "reinforce1Account", "outFuncLabel",
  "outLabel", "SystemMsg", "MsgValue1", "MsgValue2", "MsgValue3"
)

# writing a function to prepare the raw data from a single day

load_raw_csv <- function(path) {
  nthday <- read.csv2(
    file = path, sep = ";", dec = ".", header = TRUE,
    fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL
  ) %>%
    select(1:17)
  
  # renaming the columns
  if (exists("mastercolnames")) {
    colnames(nthday) <- mastercolnames
  }
  
  # extracting the relevant rows
  
  firstrow <- c()
  lastrow <- c()
  firstrow <- max(which(nthday$SystemMsg == "start")) + 1
  lastrow <- nrow(nthday)
  
  if (is.na(lastrow) | is.infinite(lastrow)) {
    lastrow <- nrow(nthday)
  }
  
  nthday <- nthday %>%
    slice(firstrow:lastrow)
}

# taking the list of days from the master table
days <- as.list(daypathlist$Day)
# taking the list of file paths from the master table
paths <- as.list(paste0(folder, daypathlist$Path))

# writing a function to aggregate the data from many days

aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    filter(day != "NA") %>% 
    mutate(day = as.numeric(day))
}

# using the functions that were written to create one big data frame of raw data

alldays <- aggregate_days(paths, days) %>%
  # removing the columns with irrelevant data 
  select(
    -sense1duration, -sense1Events, -senseRFIDrecords,
    -reinforce1Total, -reinforce1Account, -MsgValue2, -MsgValue3
  )

# reformat DateTime
alldays$DateTime <- as.POSIXct(as.numeric(alldays$DateTime) * (60 * 60 * 24),
  origin = "1899-12-30", tz = "GMT"
)

# Create a location column
alldays <- alldays %>% mutate(Loc = as.integer(str_extract(unitLabel, "[0-9]+"))) %>% 
  rename(Day = day)

# Merge the Conditions table together to get one big table with all the meta data in it
alldays <- left_join(alldays, Conditions,
  by = c("Day", "IdLabel", "Loc")
)

# Marking the choices
Allchoices_subjmean <- alldays %>%
  mutate(Discard = ifelse(str_detect(unitLabel, "pump"), 0, Discard)) %>%
  filter(Discard == 0) %>%
  mutate(
    # boolean column for whether the animal was actually detected
    detect = !is.na(Loc),
    # boolean column for whether the animal made a choice
    choice = detect & str_detect(unitLabel, "Cond"), 
    Cond = as.character(Cond)
  ) %>% 
  filter(choice == TRUE | str_detect(unitLabel, "pump")) %>% 
  mutate(Experiment = "Subjective") 

# removing the unnecessary dataframes 
rm(Conditions, daypathlist, days, paths, alldays)

#----------------------------------------------
# Preparing the training data - subjective mean 
#----------------------------------------------
# selecting the relevant data
Training_subjmean <- Allchoices_subjmean %>%
  select(-detect, -Discard, -MsgValue1) %>%
  filter(Cond == "Training", choice == "TRUE")

# Preparing the table a bit more
Training_subjmean <- Training_subjmean %>%
  mutate(
    Flower = as.numeric(str_extract(unitLabel, "[0-9]+")), # making a column for flower number
    Phase = str_remove_all(outFuncLabel, c("out1|out2")), # making a column for experiment phase
    Phase = factor(Phase, levels = c("Initial", "Forced1", "Free1", "Forced2", "Free2")),
    # making phase a factor
    vis_vol = ifelse(reinforce1value == 76, 25,
      ifelse(reinforce1value == 22, 7, 2)
    ), # making a column for the reward volume
    Flower = as.character(Flower)
  ) %>%
  group_by(Day, IdLabel) %>%
  mutate(count = n()) %>%
  filter(count > 250) %>% # filtering out those bats that did not complete the training day
  select(-count) %>%
  group_by(Day, IdLabel, Phase, Flower, vis_vol) %>%
  summarise(unitLabel = n()) # counting the visits made to the options in the different phases
#-------------------------------------------------------
# Preparing the main experimental data - subjective mean
#-------------------------------------------------------
# Selecting the relevant data
Main <- Allchoices_subjmean %>%
  select(-IdRFID, -Loc, -detect, -Discard, -MsgValue1) %>%
  filter(Cond == "Rateofchange", choice == "TRUE")

# Creating a few necessary variables
maxsine <- as.numeric(step_conv * 76)
minsine <- as.numeric(step_conv * 6)

# Preparing additional columns
Main <- Main %>%
  group_by(Day, IdLabel) %>%
  mutate(
    sine_steps = as.numeric(ifelse(outFuncLabel == "fixRewOut", SystemMsg, reinforce1value)),
    # making a column with pump steps of the wave at every decision made
    sine_vol = step_conv * sine_steps, # column with volume of the sine wave at every decision
    vis_vol = reinforce1value * step_conv, # column with volume of every visit
    timediff = ((as.numeric(difftime(DateTime, min(DateTime), units = "secs"))) / 3600),
    # calculating time elapsed in seconds
    Amplitude = as.numeric(Amplitude), # making amplitude numeric
    Disp = as.numeric(Disp), # making displacement numeric
    Period = Period / 3600000, # converting the period from seconds to hours
    chosen = as.numeric(ifelse(outFuncLabel == "fixRewOut", minsine, maxsine))
  )
# making a column with choice scaled as the peak and trough of the sine wave

# Removing the bats that did not complete the experiment
# Because not all the bats might have done the experiment in the proper order of days,
# the days need to be renumbered to remove those days that the experiment did not proceed
# properly for an animal and had to be repeated
Main_days <- Main %>% # making a new table to figure out which bats completed the experiment
  select(IdLabel, Day, Cohort, Reversal) %>%
  distinct() %>%
  ungroup() %>%
  group_by(IdLabel) %>%
  mutate(Real_day = 1:n()) %>%
  mutate(total = sum(Real_day)) %>% # summing up day number to see if it falls short of sum of 1 to 8
  mutate(complete = ifelse(total < 36, 0, 1)) # using this condition to check for completeness

Main <- left_join(Main, Main_days, by = c("Cohort", "Day", "IdLabel", "Reversal")) %>%
  # joining up the new table to the main table
  ungroup() %>%
  select(-Real_day) %>%
  group_by(Day, IdLabel) %>%
  filter(complete == 1) %>%
  select(-complete, -total) # filtering out only the bats that completed the experiment

# Marking the non-tracking bats who developed an exclusive side preference
Option_visits <- Main %>%
  mutate(Flower = ifelse(as.numeric(str_extract(unitLabel, "[0-9]+")) %% 2 == 1, "Right", "Left")) %>%
  # making a new table with the count of visits paid to the two options
  group_by(Period, IdLabel, Flower) %>%
  summarise(unitLabel = n())

Bats_l <- Option_visits %>% # a table with the visits paid to the variable option
  select(Period, IdLabel) %>%
  distinct(IdLabel) %>%
  mutate(Flower = c("Left"))
Bats_r <- Option_visits %>% # a table with the visits paid to the fixed option
  select(Period, IdLabel) %>%
  distinct(IdLabel) %>%
  mutate(Flower = c("Right"))
Nontrackers <- bind_rows(Bats_l, Bats_r) # binding the two together

Nontrackers <- left_join(Nontrackers, Option_visits, by = c("Period", "IdLabel", "Flower")) %>%
  # making a table to pull out the non-trackers
  mutate(Tracking = ifelse(unitLabel < 2 | is.na(unitLabel), "non-tracker", "tracker")) %>%
  # marking the trackers and non-trackers
  ungroup() %>%
  filter(Tracking == "non-tracker") %>% # pulling out the non-trackers
  select(IdLabel, Tracking) %>%
  distinct()

Bats_l <- Bats_l %>%
  ungroup() %>%
  select(IdLabel) %>%
  distinct() # making a table with all the bat numbers and just the bat numbers

# putting together the bat numbers and
# marked non-trackers to make one look-up table for the animals that track
Tracking <- left_join(Bats_l, Nontrackers, by = "IdLabel") %>%
  mutate(Tracking = ifelse(is.na(Tracking), "tracker", "non-tracker"))

Main_subjmean <- left_join(Main, Tracking, by = "IdLabel") # now adding the tracking label to the main data frame

# removing unnecessary dataframes
rm(Main, Main_days, Option_visits, Bats_l, Bats_r, Tracking, Nontrackers)
#------------------------------------------ 
# Preparing the pump data - subjective mean
#------------------------------------------
Pump_subjmean <- Allchoices_subjmean %>% 
  filter(SystemMsg == "start pump" | SystemMsg == "end pump")

#########################################
# Data from Experiment 2 - Objective mean
#########################################
#------------------------------------------
# Preparing the complete table of raw data
#------------------------------------------
# Load the master table
daypathlist <- read.csv2(
  file = paste0(folder, "exp2_objective_mean/meta_data/MasterTableObjectiveMean.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# Load the conditions table
Conditions <- read.csv2(
  file = paste0(folder, "exp2_objective_mean/meta_data/ConditionsObjectiveMean.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# writing a function to prepare the raw data from a single day

load_raw_csv <- function(path) {
  nthday <- read.csv2(
    file = path, sep = ";", dec = ".", header = TRUE,
    fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL
  ) %>%
    select(1:17)
  
  # renaming the columns
  if (exists("mastercolnames")) {
    colnames(nthday) <- mastercolnames
  }
  
  # extracting the relevant rows
  
  firstrow <- c()
  lastrow <- c()
  firstrow <- max(which(nthday$SystemMsg == "start")) + 1
  lastrow <- nrow(nthday)
  
  if (is.na(lastrow) | is.infinite(lastrow)) {
    lastrow <- nrow(nthday)
  }
  
  nthday <- nthday %>%
    slice(firstrow:lastrow)
}

# taking the list of days from the master table
days <- as.list(daypathlist$Day)
# taking the list of file paths from the master table
paths <- as.list(paste0(folder, daypathlist$Path))

# writing a function to aggregate the data from many days

aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    filter(day != "NA") %>% 
    mutate(day = as.numeric(day))
}

# using the functions that were written to create one big data frame of raw data

alldays <- aggregate_days(paths, days) %>%
  # removing the columns with irrelevant data 
  select(
    -sense1duration, -sense1Events, -senseRFIDrecords,
    -reinforce1Total, -reinforce1Account, -MsgValue2, -MsgValue3
  )

# reformat DateTime
alldays$DateTime <- as.POSIXct(as.numeric(alldays$DateTime) * (60 * 60 * 24),
  origin = "1899-12-30", tz = "GMT"
)

# Create a location column
alldays <- alldays %>% mutate(Loc = as.integer(str_extract(unitLabel, "[0-9]+"))) %>% 
  rename(Day = day)

# Merge the Conditions table together to get one big table with all the meta data in it
alldays <- left_join(alldays, Conditions,
  by = c("Day", "IdLabel", "Loc")
)

# Marking the choices
Allchoices_objmean <- alldays %>%
  mutate(Discard = ifelse(str_detect(unitLabel, "pump"), 0, Discard)) %>%
  filter(Discard == 0) %>%
  mutate(
    # boolean column for whether the animal was actually detected
    detect = !is.na(Loc),
    # boolean column for whether the animal made a choice
    choice = detect & str_detect(unitLabel, "Cond"),  
    Cond = as.character(Cond)
  ) %>% 
  filter(choice == TRUE | str_detect(unitLabel, "pump")) %>% 
  mutate(Experiment = "Objective") 

#---------------------------------------------
# Preparing the training data - objective mean 
#---------------------------------------------
# selecting the relevant data
Training_objmean <- Allchoices_objmean %>%
  select(-detect, -Discard, -MsgValue1) %>%
  filter(Cond == "Training")

# Preparing the table a bit more
Training_objmean <- Training_objmean %>%
  mutate(
    Flower = as.numeric(str_extract(unitLabel, "[0-9]+")), # making a column for flower number
    Phase = str_remove_all(outFuncLabel, c("out1|out2")), # making a column for experiment phase
    Phase = factor(Phase, levels = c("Initial", "Forced1", "Free1", "Forced2", "Free2")),
    # making phase a factor
    vis_vol = ifelse(reinforce1value == 76, 25, ifelse(reinforce1value == 42, 13.5, 2)), # making a column for the reward volume
    Flower = as.character(Flower)
  ) %>%
  group_by(Day, IdLabel) %>%
  mutate(count = n()) %>%
  filter(count > 250) %>% # filtering out those bats that did not complete the training day
  select(-count) %>%
  group_by(Day, IdLabel, Phase, Flower, vis_vol) %>%
  summarise(unitLabel = n()) # counting the visits made to the options in the different phases

#------------------------------------------------------
# Preparing the main experimental data - objective mean 
#------------------------------------------------------
# Selecting the relevant data
Main <- Allchoices_objmean %>%
  select(-IdRFID, -Loc, -detect, -Discard, -MsgValue1) %>%
  filter(Cond == "Rateofchange")

# Creating a few necessary variables
maxsine <- as.numeric(step_conv * 76)
minsine <- as.numeric(step_conv * 6)

# Preparing additional columns
Main <- Main %>%
  group_by(Day, IdLabel) %>%
  mutate(
    sine_steps = as.numeric(ifelse(outFuncLabel == "fixRewOut", SystemMsg, reinforce1value)),
    # making a column with pump steps of the wave at every decision made
    sine_vol = step_conv * sine_steps, # column with volume of the sine wave at every decision
    vis_vol = reinforce1value * step_conv, # column with volume of every visit
    timediff = ((as.numeric(difftime(DateTime, min(DateTime), units = "secs"))) / 3600),
    # calculating time elapsed in seconds
    Amplitude = as.numeric(Amplitude), # making amplitude numeric
    Disp = as.numeric(Disp), # making displacement numeric
    Period = Period / 3600000, # converting the period from seconds to hours
    chosen = as.numeric(ifelse(outFuncLabel == "fixRewOut", minsine, maxsine))
  )

# making a column with choice scaled as the peak and trough of the sine wave

# Removing the bats that did not complete the experiment
# Because not all the bats might have done the experiment in the proper order of days,
# the days need to be renumbered to remove those days that the experiment did not proceed
# properly for an animal and had to be repeated
Main_days <- Main %>% # making a new table to figure out which bats completed the experiment
  select(IdLabel, Day, Cohort, Reversal) %>%
  distinct() %>%
  ungroup() %>%
  group_by(IdLabel) %>%
  mutate(Real_day = 1:n()) %>%
  mutate(total = sum(Real_day)) %>% # summing up day number to see if it falls short of sum of 1 to 8
  mutate(complete = ifelse(total < 36, 0, 1))

Main <- left_join(Main, Main_days, by = c("Cohort", "Day", "IdLabel", "Reversal")) %>%
  # joining up the new table to the main table
  ungroup() %>%
  select(-Real_day) %>%
  group_by(Day, IdLabel) %>%
  filter(complete == 1) %>%
  select(-complete, -total)

# Marking the non-tracking bats who developed an exclusive side preference
Option_visits <- Main %>%
  mutate(Flower = ifelse(as.numeric(str_extract(unitLabel, "[0-9]+")) %% 2 == 1, "Right", "Left")) %>%
  # making a new table with the count of visits paid to the two options
  group_by(Period, IdLabel, Flower) %>%
  summarise(unitLabel = n())

Bats_l <- Option_visits %>% # a table with the visits paid to the variable option
  select(Period, IdLabel) %>%
  distinct(IdLabel) %>%
  mutate(Flower = c("Left"))
Bats_r <- Option_visits %>% # a table with the visits paid to the fixed option
  select(Period, IdLabel) %>%
  distinct(IdLabel) %>%
  mutate(Flower = c("Right"))
Nontrackers <- bind_rows(Bats_l, Bats_r) # binding the two together

Nontrackers <- left_join(Nontrackers, Option_visits, by = c("Period", "IdLabel", "Flower")) %>%
  # making a table to pull out the non-trackers
  mutate(Tracking = ifelse(unitLabel < 2 | is.na(unitLabel), "non-tracker", "tracker")) %>%
  # marking the trackers and non-trackers
  ungroup() %>%
  filter(Tracking == "non-tracker") %>% # pulling out the non-trackers
  select(IdLabel, Tracking) %>%
  distinct()

Bats_l <- Bats_l %>%
  ungroup() %>%
  select(IdLabel) %>%
  distinct() # making a table with all the bat numbers and just the bat numbers

Tracking <- left_join(Bats_l, Nontrackers, by = "IdLabel") %>%
  mutate(Tracking = ifelse(is.na(Tracking), "tracker", "non-tracker"))
# putting together the bat numbers and
# marked non-trackers to make one look-up table for the animals that track

# now adding the tracking label to the main data frame
Main_objmean <- left_join(Main, Tracking, by = "IdLabel") 

Main_objmean <- Main_objmean %>%
  mutate(
    Tracking = as.character(Tracking),
    Tracking = ifelse(IdLabel == "Bat92", "non-tracker", Tracking)
  )


rm(daypathlist, Conditions, days, paths, alldays, Main, Main_days, Option_visits, Bats_l, Bats_r, Tracking, Nontrackers)

#------------------------------------------ 
# Preparing the pump data - objective mean
#------------------------------------------
Pump_objmean <- Allchoices_objmean %>% 
  filter(SystemMsg == "start pump" | SystemMsg == "end pump")

#########
#Putting together the data tables 
#########
# binding together the training data tables from the two experiments 
Training <- bind_rows(Training_subjmean, Training_objmean)

# binding together the main data tables from the two experiments 
Main <- bind_rows(Main_subjmean, Main_objmean)

########
# Writing the required CSV files 
#######
# creating a CSV file including the pump data for the training days
write.csv2(Pump_subjmean, file = paste0(folder, "processed_data/Pump_subj.csv"), row.names = FALSE)

# creating a CSV file including the pump data for the main days 
write.csv2(Pump_objmean, file = paste0(folder, "processed_data/Pump_obj.csv"), row.names = FALSE)

# creating a CSV file with the training data
write.csv2(Training, file = paste0(folder, "processed_data/Training_roc.csv"), row.names = FALSE)

# Creating a CSV file with the main experimental data
write.csv2(Main, file = paste0(folder, "processed_data/Main_roc.csv"), row.names = FALSE)
