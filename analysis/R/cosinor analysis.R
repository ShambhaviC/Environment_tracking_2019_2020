rm(list = ls())

library(season)
library(tidyverse)
library(brms)
library(bayesplot)
library(lubridate)
library(broom)

#######
# Reading in and preparing data and fitting the models
########

# marking the folder 
folder <- "/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/analysis/"

Main_all <- read.csv2(
  file = paste0(folder, "data/processed_data/Main_roc.csv"),
  header = TRUE)

# setting the parameters for volume calculation, sine wave calculation and pump-step conversions
# pump steps for the maximum volume of the fluctuating option
maxsine_steps <- 76
# pump steps for the minimum volume of the fixed option
minsine_steps <- 6
# conversion factor between pump-steps and microLitres of volume
step_conv <- 0.324
# converting the rewards from pump-step units to microLitres
maxsine <- step_conv * maxsine_steps
minsine <- step_conv * minsine_steps
# setting the fixed volumes as variables
fixed1 <- step_conv * 22
fixed2 <- step_conv * 42
# setting the pump steps for the fixed option
fixed_steps <- 22 

# calculating the sine-wave for the bats, with the actual start time of the wave instead of when the bat made its first rewarded visit at the fluctuating option, as explained in the supplementary information

Main_all <- Main_all %>% 
  # adding the pump steps for the two fixed options
  mutate(Fixed_steps = ifelse(Experiment == "Subjective", 22, 42), 
         Fixed_vol = Fixed_steps * step_conv)

# inserting a space between the word "Bat" and the number of the animal
Main_all <- Main_all %>% 
  mutate(Bat_word = ifelse((str_detect(IdLabel, "Bat") == TRUE), "Bat", ""), 
         Bat_number = ifelse((str_detect(IdLabel, "Bat") == TRUE), as.integer(str_extract(IdLabel, "[0-9]+")), IdLabel), 
         IdLabel = ifelse(is.na(IdLabel), IdLabel, paste0(Bat_word, " ", Bat_number))) %>% 
  select(-Bat_word, -Bat_number)

# creating a lookup table to find the first occurrence of a visit to the flower that was fluctuating on a particular day 

fluc_visits <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, Day, IdLabel, unitLabel) %>% 
  select(Experiment, Day, IdLabel, unitLabel, outFuncLabel) %>% 
  distinct() %>% 
  filter(str_detect(outFuncLabel, "sine") | str_detect(outFuncLabel, "fix"))

# joining the lookup table to the Main table to correctly mark the unrewarded visits as fixed or fluctuating 

Main_all <- Main_all %>% 
  # first removing the original column
  select(-outFuncLabel)

Main_all <- left_join(Main_all, fluc_visits, by = c("Experiment", "Day", "IdLabel", "unitLabel")) 

# removing the now unnecessary look-up table 
rm(fluc_visits)

# adding a period-day column to the table

Main_all <- Main_all %>%
  mutate(
    Rev = "Rev",
    Period_day = ifelse(Reversal == 0, Period, paste0(as.character(Period), " ", Rev))
  ) %>%
  # removing the now redundant columns
  select(-Rev, -Reversal)

# creating a look-up table for calculating the sine wave

sinewave <- Main_all %>%
  # filtering only the visits to the fluctuating output
  filter(outFuncLabel == "sineRewOut") %>%
  # selecting relevant columns from the main table to calculate the sine wave
  select(Experiment, Day, IdLabel, Period, Period_day, timediff, Amplitude, Disp, Fixed_steps, vis_vol, Tracking) %>%
  group_by(Experiment, Day, IdLabel) %>%
  mutate(
    Period = as.numeric(str_remove(Period_day, "Rev")), 
    # adding a row counter
    rown = 1:n(),
    # adding a column with the number of points on the calculated sine wave
    reps = 360
  ) %>%
  # taking the first occurrence of the fluctuating output
  filter(rown == 1)

# noting the number of columns in the data frame
nsine <- as.numeric(ncol(sinewave))

# repeating the rows by the number of points on the wave so the whole sine wave can be calculated
sinewave <- sinewave[rep(row.names(sinewave), sinewave$reps), 1:nsine]

# adding a column with the actual time increments
sinewave <- sinewave %>%
  # removing the old row counter so it can be done over
  select(-rown) %>%
  group_by(Experiment, Day, IdLabel) %>%
  mutate(
    # calculating the row counter again
    rown = 1:n(),
    # calculating the time differences with the proper increments: 12 hours are divided among 360 points on the wave
    timediff = ifelse(rown == 1, timediff, (lag(timediff) + (12 / 360) * (rown - 1))),
    # creating a dummy time column to calculate the wave so it starts at the peak regardless of the timediff column
    wavetime = 0,
    wavetime = ifelse(rown == 1, 0, (lag(wavetime) + (12 / 360) * (rown - 1))),
    # converting the time values back to seconds
    Period = Period * 3600,
    # converting the dummy time column to seconds
    wavetime = wavetime * 3600,
    # calculating the sine wave values and converting from pump step values to microLitres
    sine_vol = (Amplitude * sin(2 * pi * (1 / Period) * wavetime + (pi / 2)) + Disp) * step_conv,
    Period = Period / 3600
  ) %>%
  # removing time points that occurred after 12 hours by the calculation
  filter(timediff <= 12) %>%
  # removing the now-unnecessary columns
  select(-wavetime, -reps, -rown)

#####
# Subjective mean
#####
rhythmicity_subj <- Main_all %>% 
  filter(Tracking == "tracker", 
         Experiment == "Subjective") %>% 
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0)) %>% 
  select(DateTime, IdLabel, Period, sine_choice) %>% 
  ungroup() %>% 
  group_by(IdLabel, Period) %>% 
  # nest(Date = Date,
  #      sine_choice = sine_choice) %>%
  mutate(cycles = 24/Period) %>% 
  mutate(DateTime = as.POSIXct(DateTime),
         sine_choice = as.integer(sine_choice)) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel)

lookup <- rhythmicity_subj %>%
  select(Period, IdLabel) %>%
  distinct() %>%
  ungroup()

######
#Bat 22
#######
Bat_22_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 22") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_22_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 22") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_22_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 22") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_22_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 22") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_22_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_22_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 22", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_22_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_22_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 22", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_22_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_22_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 22", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_22_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_22_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 22", 
         Period = 6)

Bat_22 <- bind_rows (Bat_22_0.75, Bat_22_1.5, Bat_22_3, Bat_22_6)

rm(Bat_22_0.75, Bat_22_1.5, Bat_22_3, Bat_22_6)

######
# Bat 45 
######
Bat_45_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 45") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_45_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 45") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_45_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 45") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_45_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 45") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_45_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_45_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 45", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_45_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_45_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 45", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_45_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_45_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 45", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_45_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_45_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 45", 
         Period = 6)

Bat_45 <- bind_rows (Bat_45_0.75, Bat_45_1.5, Bat_45_3, Bat_45_6)

rm(Bat_45_0.75, Bat_45_1.5, Bat_45_3, Bat_45_6)

######
# Bat 43
######

Bat_43_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_43_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_43_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_43_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_43_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 6)

Bat_43 <- bind_rows (Bat_43_0.75, Bat_43_1.5, Bat_43_3, Bat_43_6)

rm(Bat_43_0.75, Bat_43_1.5, Bat_43_3, Bat_43_6)

######
# Bat 100
######
Bat_100_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 100") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_100_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 100") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_100_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 100") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_100_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 100") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_100_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_100_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 100", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_100_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_100_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 100", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_100_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_100_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 100", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_100_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_100_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 100", 
         Period = 6)

Bat_100 <- bind_rows (Bat_100_0.75, Bat_100_1.5, Bat_100_3, Bat_100_6)

rm(Bat_100_0.75, Bat_100_1.5, Bat_100_3, Bat_100_6)

#####
# Bat 103
#####

Bat_103_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 103") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_103_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 103") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_103_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 103") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_103_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 103") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_103_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_103_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 103", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_103_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_103_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 103", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_103_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_103_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 103", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_103_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_103_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 103", 
         Period = 6)

Bat_103 <- bind_rows (Bat_103_0.75, Bat_103_1.5, Bat_103_3, Bat_103_6)

rm(Bat_103_0.75, Bat_103_1.5, Bat_103_3, Bat_103_6)

#####
# Bat 16
#####

Bat_16_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 16") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_16_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 16") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_16_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 16") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_16_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 16") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_16_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_16_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 16", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_16_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_16_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 16", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_16_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_16_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 16", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_16_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_16_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 16", 
         Period = 6)

Bat_16 <- bind_rows (Bat_16_0.75, Bat_16_1.5, Bat_16_3, Bat_16_6)

rm(Bat_16_0.75, Bat_16_1.5, Bat_16_3, Bat_16_6)

#####
# Bat 30
######

Bat_30_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 30") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_30_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 30") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_30_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 30") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_30_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 30") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_30_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_30_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 30", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_30_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_30_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 30", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_30_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_30_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 30", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_30_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_30_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 30", 
         Period = 6)

Bat_30 <- bind_rows (Bat_30_0.75, Bat_30_1.5, Bat_30_3, Bat_30_6)

rm(Bat_30_0.75, Bat_30_1.5, Bat_30_3, Bat_30_6)

#####
# Bat 46
#####
Bat_46_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 46") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_46_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 46") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_46_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 46") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_46_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 46") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_46_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_46_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 46", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_46_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_46_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 46", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_46_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_46_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 46", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_46_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_46_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 46", 
         Period = 6)

Bat_46 <- bind_rows (Bat_46_0.75, Bat_46_1.5, Bat_46_3, Bat_46_6)

rm(Bat_46_0.75, Bat_46_1.5, Bat_46_3, Bat_46_6)

#####
# Bat 67 
#####
Bat_67_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 67") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_67_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 67") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_67_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 67") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_67_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 67") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_67_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_67_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 67", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_67_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_67_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 67", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_67_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_67_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 67", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_67_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_67_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 67", 
         Period = 6)

Bat_67 <- bind_rows (Bat_67_0.75, Bat_67_1.5, Bat_67_3, Bat_67_6)

rm(Bat_67_0.75, Bat_67_1.5, Bat_67_3, Bat_67_6)

##### 
# Bat 77
#####

Bat_77_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 77") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_77_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 77") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_77_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 77") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_77_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 77") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_77_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_77_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 77", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_77_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_77_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 77", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_77_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_77_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 77", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_77_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_77_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 77", 
         Period = 6)

Bat_77 <- bind_rows (Bat_77_0.75, Bat_77_1.5, Bat_77_3, Bat_77_6)

rm(Bat_77_0.75, Bat_77_1.5, Bat_77_3, Bat_77_6)

######
# Bat 80
######

Bat_80_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 80") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_80_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 80") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_80_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 80") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_80_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 80") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_80_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_80_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 80", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_80_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_80_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 80", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_80_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_80_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 80", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_80_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_80_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 80", 
         Period = 6)

Bat_80 <- bind_rows (Bat_80_0.75, Bat_80_1.5, Bat_80_3, Bat_80_6)

rm(Bat_80_0.75, Bat_80_1.5, Bat_80_3, Bat_80_6)

#####
# Bat 95
#####
Bat_95_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 95") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_95_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 95") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_95_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 95") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_95_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 95") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_95_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_95_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 95", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_95_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_95_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 95", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_95_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_95_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 95", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_95_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_95_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 95", 
         Period = 6)

Bat_95 <- bind_rows (Bat_95_0.75, Bat_95_1.5, Bat_95_3, Bat_95_6)

rm(Bat_95_0.75, Bat_95_1.5, Bat_95_3, Bat_95_6)

#####
rhythmicity_subj_results <- bind_rows(Bat_22, Bat_45, Bat_43, Bat_100, Bat_103,
                                      Bat_16, Bat_30, Bat_46, Bat_67, Bat_77, 
                                      Bat_80, Bat_95)

rm(Bat_22, Bat_45, Bat_43, Bat_100, Bat_103,
   Bat_16, Bat_30, Bat_46, Bat_67, Bat_77, 
   Bat_80, Bat_95)

######
# Objective mean
######
rhythmicity_obj <- Main_all %>% 
  filter(Tracking == "tracker", 
         Experiment == "Objective") %>% 
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0)) %>% 
  select(DateTime, IdLabel, Period, sine_choice) %>% 
  ungroup() %>% 
  group_by(IdLabel, Period) %>% 
  # nest(Date = Date,
  #      sine_choice = sine_choice) %>%
  mutate(cycles = 24/Period) %>% 
  mutate(DateTime = as.POSIXct(DateTime),
         sine_choice = as.integer(sine_choice)) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel)

lookup <- rhythmicity_obj %>%
  select(Period, IdLabel) %>%
  distinct() %>%
  ungroup()

######
#Bat 101
#######
Bat_101_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 101") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_101_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 101") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_101_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 101") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_101_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 101") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_101_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_101_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 101", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_101_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_101_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 101", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_101_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_101_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 101", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_101_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_101_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 101", 
         Period = 6)

Bat_101 <- bind_rows (Bat_101_0.75, Bat_101_1.5, Bat_101_3, Bat_101_6)

rm(Bat_101_0.75, Bat_101_1.5, Bat_101_3, Bat_101_6)


######
# Bat 105
######
Bat_105_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 105") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_105_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 105") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_105_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 105") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_105_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 105") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_105_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_105_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 105", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_105_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_105_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 105", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_105_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_105_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 105", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_105_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_105_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 105", 
         Period = 6)

Bat_105 <- bind_rows (Bat_105_0.75, Bat_105_1.5, Bat_105_3, Bat_105_6)

rm(Bat_105_0.75, Bat_105_1.5, Bat_105_3, Bat_105_6)

######
# Bat 5
######
Bat_5_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 5") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_5_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 5") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_5_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 5") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_5_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 5") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_5_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_5_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 5", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_5_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_5_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 5", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_5_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_5_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 5", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_5_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_5_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 5", 
         Period = 6)

Bat_5 <- bind_rows (Bat_5_0.75, Bat_5_1.5, Bat_5_3, Bat_5_6)

rm(Bat_5_0.75, Bat_5_1.5, Bat_5_3, Bat_5_6)

######
# Bat 50 
######
Bat_50_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 50") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_50_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 50") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_50_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 50") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_50_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 50") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_50_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_50_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 50", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_50_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_50_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 50", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_50_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_50_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 50", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_50_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_50_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 50", 
         Period = 6)

Bat_50 <- bind_rows (Bat_50_0.75, Bat_50_1.5, Bat_50_3, Bat_50_6)

rm(Bat_50_0.75, Bat_50_1.5, Bat_50_3, Bat_50_6)

######
# Bat 1
######
Bat_1_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 1") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_1_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 1") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_1_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 1") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_1_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 1") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_1_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_1_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 1", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_1_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_1_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 1", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_1_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_1_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 1", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_1_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_1_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 1", 
         Period = 6)

Bat_1 <- bind_rows (Bat_1_0.75, Bat_1_1.5, Bat_1_3, Bat_1_6)

rm(Bat_1_0.75, Bat_1_1.5, Bat_1_3, Bat_1_6)

#######
# Bat 19
#######
Bat_19_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 19") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_19_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 19") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_19_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 19") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_19_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 19") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_19_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_19_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 19", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_19_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_19_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 19", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_19_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_19_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 19", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_19_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_19_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 19", 
         Period = 6)

Bat_19 <- bind_rows (Bat_19_0.75, Bat_19_1.5, Bat_19_3, Bat_19_6)

rm(Bat_19_0.75, Bat_19_1.5, Bat_19_3, Bat_19_6)
######
# Bat 3
######
Bat_3_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 3") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_3_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 3") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_3_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 3") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_3_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 3") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_3_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_3_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 3", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_3_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_3_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 3", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_3_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_3_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 3", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_3_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_3_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 3", 
         Period = 6)

Bat_3 <- bind_rows (Bat_3_0.75, Bat_3_1.5, Bat_3_3, Bat_3_6)

rm(Bat_3_0.75, Bat_3_1.5, Bat_3_3, Bat_3_6)
######
# Bat 39
######
Bat_39_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 39") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_39_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 39") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_39_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 39") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_39_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 39") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_39_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_39_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 39", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_39_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_39_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 39", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_39_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_39_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 39", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_39_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_39_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 39", 
         Period = 6)

Bat_39 <- bind_rows (Bat_39_0.75, Bat_39_1.5, Bat_39_3, Bat_39_6)

rm(Bat_39_0.75, Bat_39_1.5, Bat_39_3, Bat_39_6)
######
# Bat 43
######
Bat_43_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_43_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 43") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_43_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_43_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_43_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_43_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_43_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 43", 
         Period = 6)

Bat_43 <- bind_rows (Bat_43_0.75, Bat_43_1.5, Bat_43_3, Bat_43_6)

rm(Bat_43_0.75, Bat_43_1.5, Bat_43_3, Bat_43_6)
######
# Bat 4
######
Bat_4_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 4") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_4_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 4") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_4_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 4") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_4_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 4") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_4_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_4_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 4", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_4_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_4_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 4", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_4_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_4_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 4", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_4_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_4_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 4", 
         Period = 6)

Bat_4 <- bind_rows (Bat_4_0.75, Bat_4_1.5, Bat_4_3, Bat_4_6)

rm(Bat_4_0.75, Bat_4_1.5, Bat_4_3, Bat_4_6)
######
# Bat 47
######
Bat_47_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 47") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_47_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 47") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_47_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 47") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_47_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 47") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_47_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_47_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 47", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_47_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_47_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 47", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_47_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_47_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 47", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_47_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_47_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 47", 
         Period = 6)

Bat_47 <- bind_rows (Bat_47_0.75, Bat_47_1.5, Bat_47_3, Bat_47_6)

rm(Bat_47_0.75, Bat_47_1.5, Bat_47_3, Bat_47_6)
######
# Bat 68
######
Bat_68_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 68") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_68_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 68") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_68_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 68") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_68_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 68") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_68_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_68_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 68", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_68_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_68_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 68", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_68_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_68_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 68", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_68_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_68_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 68", 
         Period = 6)

Bat_68 <- bind_rows (Bat_68_0.75, Bat_68_1.5, Bat_68_3, Bat_68_6)

rm(Bat_68_0.75, Bat_68_1.5, Bat_68_3, Bat_68_6)
######
# Bat 69
######
Bat_69_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 69") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_69_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 69") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_69_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 69") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_69_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 69") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_69_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_69_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 69", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_69_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_69_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 69", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_69_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_69_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 69", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_69_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_69_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 69", 
         Period = 6)

Bat_69 <- bind_rows (Bat_69_0.75, Bat_69_1.5, Bat_69_3, Bat_69_6)

rm(Bat_69_0.75, Bat_69_1.5, Bat_69_3, Bat_69_6)
######
# Bat 70
######
Bat_70_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 70") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_70_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 70") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_70_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 70") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_70_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 70") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_70_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_70_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 70", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_70_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_70_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 70", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_70_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_70_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 70", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_70_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_70_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 70", 
         Period = 6)

Bat_70 <- bind_rows (Bat_70_0.75, Bat_70_1.5, Bat_70_3, Bat_70_6)

rm(Bat_70_0.75, Bat_70_1.5, Bat_70_3, Bat_70_6)
######
# Bat 74
######
Bat_74_0.75 <- Main_all %>%
  filter(Period == 0.75, 
         IdLabel == "Bat 74") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_74_1.5 <- Main_all %>%
  filter(Period == 1.5, 
         IdLabel == "Bat 74") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_74_3 <- Main_all %>%
  filter(Period == 3, 
         IdLabel == "Bat 74") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

Bat_74_6 <- Main_all %>%
  filter(Period == 6, 
         IdLabel == "Bat 74") %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
    DateTime = as.POSIXct(DateTime),
    sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
  )

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_74_0.75, 
               type = 'hourly', 
               cycles = 32,
               family = binomial(link='cloglog')
)

Bat_74_0.75 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 74", 
         Period = 0.75)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_74_1.5, 
               type = 'hourly', 
               cycles = 16,
               family = binomial(link='cloglog')
)

Bat_74_1.5 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 74", 
         Period = 1.5)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_74_3, 
               type = 'hourly', 
               cycles = 8,
               family = binomial(link='cloglog')
)

Bat_74_3 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 74", 
         Period = 3)

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = Bat_74_6, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

Bat_74_6 <- tidy(res$glm) %>% 
  mutate(IdLabel = "Bat 74", 
         Period = 6)

Bat_74 <- bind_rows (Bat_74_0.75, Bat_74_1.5, Bat_74_3, Bat_74_6)

rm(Bat_74_0.75, Bat_74_1.5, Bat_74_3, Bat_74_6)
######
rhythmicity_obj_results <- bind_rows(Bat_101, Bat_105, Bat_5, Bat_50, Bat_1,
                                      Bat_19, Bat_3, Bat_39, Bat_43, Bat_4, 
                                      Bat_47, Bat_68, Bat_69, Bat_70, Bat_74)

rm(Bat_101, Bat_105, Bat_5, Bat_50, Bat_1,
   Bat_19, Bat_3, Bat_39, Bat_43, Bat_4, 
   Bat_47, Bat_68, Bat_69, Bat_70, Bat_74)

#######

rhythmicity_subj_results <- rhythmicity_subj_results %>% 
  mutate_at(c("estimate", "std.error", "statistic", "p.value"), ~round(., digits= 3)) %>% 
  mutate(sig_level = case_when(p.value >= 0.05 ~ "n.s.", 
                                  p.value < 0.05 & p.value >= 0.01 ~ "*", 
                                  p.value < 0.01 & p.value >= 0.001 ~ "**", 
                                  p.value < 0.001 ~ "***"), 
         count = case_when(p.value >= 0.05 | term == "(Intercept)" ~ 0, 
                                  p.value < 0.05 & p.value >= 0.01 & term != "(Intercept)" ~ 1, 
                                  p.value < 0.01 & p.value >= 0.001 & term != "(Intercept)" ~ 1, 
                                  p.value < 0.001 & term != "(Intercept)" ~ 1)) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel) %>% 
  mutate(count = sum(count), 
         significant = ifelse(count > 0, "TRUE", "FALSE")) %>% 
  select(-count)

count_sig_subj <- rhythmicity_subj_results %>% 
  ungroup() %>% 
  distinct() %>%
  group_by(Period, significant) %>% 
  summarise (count = n())
  
rhythmicity_obj_results <- rhythmicity_obj_results %>% 
  mutate_at(c("estimate", "std.error", "statistic", "p.value"), ~round(., digits= 3)) %>% 
  mutate(sig_level = case_when(p.value >= 0.05 ~ "n.s.", 
                               p.value < 0.05 & p.value >= 0.01 ~ "*", 
                               p.value < 0.01 & p.value >= 0.001 ~ "**", 
                               p.value < 0.001 ~ "***"), 
         count = case_when(p.value >= 0.05 | term == "(Intercept)" ~ 0, 
                           p.value < 0.05 & p.value >= 0.01 & term != "(Intercept)" ~ 1, 
                           p.value < 0.01 & p.value >= 0.001 & term != "(Intercept)" ~ 1, 
                           p.value < 0.001 & term != "(Intercept)" ~ 1)) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel) %>% 
  mutate(count = sum(count), 
         significant = ifelse(count > 0, "TRUE", "FALSE")) %>% 
  select(-count)

count_sig_obj <- rhythmicity_obj_results %>% 
  ungroup() %>% 
  distinct() %>%
  group_by(Period, significant) %>% 
  summarise (count = n())

folder <- "/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/analysis/data/"

write.csv2(rhythmicity_subj_results, file = paste0(folder, "processed_data/rhythmicity_subj_results.csv"), row.names = FALSE)
write.csv2(rhythmicity_obj_results, file = paste0(folder, "processed_data/rhythmicity_obj_results.csv"), row.names = FALSE)

