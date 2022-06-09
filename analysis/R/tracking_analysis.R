rm(list = ls())

#library(season)
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

######
# binning into 5 minute bins 
#######
# Objective mean
binratio <- 1/18 #set this for a binsize that is a proportion of the period
binsize_min <- 5 #set this for a fixed binsize in units minutes
binsize_sec <- binsize_min*60 #calculating fixed binsize in seconds
#this is the only variable that needs to be set for the analyses below:
#the size of the bin for the data as a ratio of the wave period. Here it is set as 1/18
binsize <- data.frame(Period = c(0.75, 1.5, 3, 6))
binsize <- binsize %>%
  mutate(Period_min = Period*60, 
         prop_binsize = 60*round(Period*60*binratio),
         prop_binnumber = 3600*12/prop_binsize,
         fixed_binsize = binsize_sec, #this column for whichever actual fixed binsize is used in 
         fixed_binnumber = 3600*12/binsize_sec) #binsize is set equal for all the periods for now

#Creating a table with the information about periods and days
Datetimes <- Main_all %>% 
  filter(Experiment == "Objective") %>% 
  ungroup() %>%
  select(Day, Period_day, IdLabel, DateTime, Tracking) %>%
  #mutate(day = day(DateTime)) %>%
  select(-DateTime) %>%
  distinct() %>%
  arrange(IdLabel)

#this is the code for setting the binsize as fixed across all periods. The code for doing it proportionally is not yet written
timeseries1 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-19 15:00:00"), to = as.POSIXct("2020-06-27 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries2 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-29 15:00:00"), to = as.POSIXct("2020-07-10 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries3 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-07-16 15:00:00"), to = as.POSIXct("2020-07-24 03:00:00"), by = as.numeric(binsize[1,5])))

timeseries <- bind_rows(timeseries1, timeseries2, timeseries3) %>%  
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  filter(Hour >= 15 | Hour < 3) %>% #Filtering and removing the non-experimental times
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
seq <- c(2:9, 11:21, 23:30)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins) %>% 
  filter(!is.na(time_bins)) %>% 
  mutate(time_bins = as.character(time_bins))

rm(timeseries1, timeseries2, timeseries3)

Main_all <- Main_all %>%
  group_by(Period_day, IdLabel) %>%
  mutate(time_bins = floor_date(as_datetime(DateTime), unit = "5 minute"))

#Creating a separate table for the workrate binning
fivemin_bins_obj_intm <- Main_all %>%
  filter(Experiment == "Objective", 
         #Tracking == "tracker"
         ) %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking) %>% 
  mutate(time_bins = as.character(time_bins))

#Finally, adding the time series data to the experimental data
fivemin_bins_obj <- left_join(timeseries, fivemin_bins_obj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(time_bins = as.POSIXct(time_bins), 
         Hour = hour(time_bins)) %>%
  filter(Hour < 3 | Hour >= 15) %>%
  select(-Hour) %>% 
  mutate(Experiment = "Objective")

# Subjective mean 

#Creating a table with the information about periods and days
Datetimes <- Main_all %>% 
  filter(Experiment == "Subjective") %>% 
  ungroup() %>%
  select(Day, Period_day, IdLabel, DateTime, Tracking) %>%
  #mutate(day = day(DateTime)) %>%
  select(-DateTime) %>%
  unique() %>%
  arrange(IdLabel)

timeseries_1 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2019-12-01 14:00:00"), to = as.POSIXct("2019-12-09 02:00:00"), by = as.numeric(binsize[1,5]))) 
timeseries_2 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2019-12-11 14:00:00"), to = as.POSIXct("2019-12-20 02:00:00"), by = as.numeric(binsize[1,5]))) 
timeseries_3 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2019-12-22 14:00:00"), to = as.POSIXct("2019-12-31 02:00:00"), by = as.numeric(binsize[1,5]))) 

#Note: need separate code when binning as a proportion of the period 
timeseries <- bind_rows(timeseries_1, timeseries_2, timeseries_3) %>%
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  filter(Hour >= 14 | Hour < 2) %>% #Filtering and removing the non-experimental times
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
seq <- c(2:9,11:19,21:29)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

#Now adding the information about the periods to the timeseries and removing NAs
timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins) %>% 
  mutate(time_bins = as.character(time_bins))

#Creating a separate table for the workrate binning
fivemin_bins_subj_intm <- Main_all %>%
  filter(Experiment == "Subjective", 
         #Tracking == "tracker"
         ) %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking) %>% 
  mutate(time_bins = as.character(time_bins))

# Finally, adding the time series data to the experimental data
fivemin_bins_subj <- left_join(timeseries, fivemin_bins_subj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(time_bins = as.POSIXct(time_bins), 
         Hour = hour(time_bins)) %>%
  filter(Hour < 2 | Hour >= 14) %>%
  select(-Hour) %>%
  mutate(Experiment = "Subjective")

rm(fivemin_bins_subj_intm, fivemin_bins_obj_intm)
fivemin_bins <- bind_rows(fivemin_bins_subj, fivemin_bins_obj)

# fivemin_bins_summ <- fivemin_bins %>% 
#   ungroup() %>% 
#   group_by(Experiment, Period_day, IdLabel, time_bins) %>%
#   summarise(count = n()) %>% 
#   ungroup() %>% 
#   group_by(Experiment, Period_day) %>% 
#   summarise(median = median(count), 
#             mean = mean(count))

# setting the minimum and maximum sine volume 
min_sinevol <- 1.944
max_sinevol <- 24.624

# calculating the fixed volume 
fixed_vol_subj <- (7.128 - min_sinevol)/(max_sinevol - min_sinevol)
fixed_vol_obj <- (13.608 - min_sinevol)/(max_sinevol - min_sinevol)

fivemin_bins <- fivemin_bins %>% 
  filter(!is.na(sine_vol)) %>% 
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0))

fivemin_bins_averages <- fivemin_bins %>% 
  ungroup() %>%
  group_by(Experiment, Tracking, Period_day, Period, IdLabel, time_bins) %>% 
  summarise(mean_sine_choice = mean(sine_choice)) %>% 
  mutate(timediff = time_bins - lag(time_bins), 
         timediff = ifelse(is.na(timediff), 0, as.numeric(str_extract(timediff, "[0-9]+"))), 
         timediff = timediff * 60,
         timediff = cumsum(timediff), 
         timediff = ifelse(timediff != 0, timediff - 150, timediff), 
         Period = Period * 3600) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(sine_vol = 0.5 * sin(2 * pi * (1 / Period) * timediff + (pi / 2)) + 0.5, 
         fixed_vol = ifelse(Experiment == "Objective", fixed_vol_obj, fixed_vol_subj), 
         ideal_bat = ifelse(sine_vol > fixed_vol, 1, 0))

# Single bat all periods summary plot
fivemin_bins_averages %>% 
  filter(IdLabel == "Bat 46", 
         #Experiment == "Objective", 
         #Period == 21600
         ) %>% 
  ggplot() +
  geom_line(aes(timediff, sine_vol)) + 
  geom_line(aes(timediff, mean_sine_choice), colour = "red") + 
  geom_line(aes(timediff, ideal_bat), colour = "blue") + 
  #geom_hline(yintercept = fixed_vol_subj, linetype = "dashed") + 
  geom_hline(aes(yintercept = fixed_vol), linetype = "dashed") + 
  ylab("Prop. fluctuating visits and \n choice for fluctuating option") + 
  xlab("Time") + 
  facet_wrap(Period_day~.) + 
  theme_bw()

check_3 <- fivemin_bins_averages %>%
  ungroup() %>% 
  select(Experiment, Period_day, IdLabel) %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  summarise(count = n()) 
  # ungroup() %>% 
  # group_by(Experiment) %>% 
  # summarise(mean_count = mean(count), 
  #           sd_ccount = sd(count))

#######
# Vladi's goodness of fit code
#######

x1 <- seq(0, 100, by = 0.1)
y1 <- sin(2 * pi * 0.2 * x1) # "true" values
obs1 <- sin(2*pi* 0.2 * lag(x1, 30)) + rnorm(n = length(x1), sd = 0.1) # observed values
sine <- tibble(x = x1, y = y1, obs = obs1) # put data into tibble
# plot observed and theoretical, note lag
sine %>%
  ggplot() +
  geom_line(aes(x, y), color = "orchid") +
  geom_line(aes(x, obs))

# helper functions for calculating goodness of fit (gof) measures
rmse <- function(obs, pred) {
  sqrt(mean(obs - pred, na.rm = TRUE)^2)
}

nrmse <- function(obs, pred) {
  denom <- sd(obs, na.rm = TRUE)
  if (denom == 0) denom <- 0.000001
  rmse(obs, pred)/denom
}

rae <- function(obs, pred) {
  mean_obs <- mean(obs, na.rm = TRUE)
  denom <- sum(abs(mean_obs - obs), na.rm = TRUE)
  if (denom == 0) denom <- 0.000001
  sum(abs(obs - pred), na.rm = TRUE) / denom
}

mape <- function(obs, pred) {
  n_min <- min(sum(!is.na(obs)),
               sum(!is.na(pred)))
  sum(abs((obs - pred) / obs), na.rm = TRUE) / n_min
}

# wrapper for calculating gof measure for given function at given lag

get_gof <- function(obs, pred, gof_func, lag_n) {
  if (length(lag_n) < 2) { # for a single lag value return simple output
    return(gof_func(obs, lag(pred, lag_n)))
    # since the observed vector could have missing values, the predicted
    # is shifted (lagged) instead
  } else { # for a range of lag values return tibble with lags
    map_dbl(lag_n, ~gof_func(obs, lag(pred, .x))) %>%
      as_tibble() %>%
      mutate(lag_n = lag_n)
  }
}
# for a range of lag values, return the lag with the best gof measure

get_best_gof <- function(obs, pred, got_func, lag_n = 0) {
  
  if (length(lag_n) == 1) {
    if (lag_n == "max") {
      n_min <- min(sum(!is.na(obs)), 
                   sum(!is.na(pred)))
      
      lag_n <- 1:n_min
    }
    
  }
  
  fun_label <- as_label(enquo(got_func))
  lag_label <- paste0(fun_label, "_lag_n")
  
  res <- get_gof(obs, pred, got_func, lag_n) %>%
    as_tibble() %>% 
    slice_min(value, with_ties = FALSE) %>%
    rename(!!fun_label := value)
  
  if (length(res) == 1) {
    return(res)
  }
  
  res %>%
    rename(!!lag_label := lag_n)
  
}

# get_best_gof <- function(obs, pred, got_func, lag_n = 0) {
#   
#   if (length(lag_n) == 1) {
#     if (lag_n == "max") {
#       n_min <- min(sum(!is.na(obs)), 
#                    sum(!is.na(pred)))
#       
#       lag_n <- 1:n_min
#     }
#     
#   }
#   
#   fun_label <- as_label(enquo(got_func))
#   lag_label <- paste0(fun_label, "_lag_n")
#   
#   res <- get_gof(obs, pred, got_func, lag_n) %>%
#     as_tibble() %>% 
#     filter(value == min(value)) %>%
#     rename(!!fun_label := value)
#   
#   if (length(res) == 1) {
#     return(res)
#   }
#   
#   res %>%
#     rename(!!lag_label := lag_n)
#   
# }

gof1 <- get_gof(obs1, y1, rmse, lag_n = 1:100)
rae1 <- get_gof(obs1, y1, rae, lag_n = 1:100)
mape1 <- get_gof(obs1, y1, mape, lag_n = 1:100)
nrmse1 <- get_gof(obs1, y1, nrmse, lag_n = 1:100)

get_best_gof(obs1, y1, nrmse, lag_n = 0:0)

# plot gof values for different gof functions and different lag values 
get_gof(obs1, y1, nrmse, lag_n = 1:100) %>%
  ggplot(aes(lag_n, value)) +
  geom_point() +
  # geom_point(data = gof1, color = "red") +
  geom_point(data = rae1, color = "green") 
  # geom_point(data = nrmse1, color = "blue") + 
  # geom_point(data = mape1, color = "purple")

# check whether the best gofs get correctly estimated
sine %>%
  summarise(get_best_gof(obs, y, mape, 1:100),
            get_best_gof(obs, y, rae, 1:100), 
            get_best_gof(obs, y, nrmse, 1:100))

######
# applying gofs to the real data
#######
# single lag value
check <- fivemin_bins_averages %>% 
  filter(IdLabel == "Bat 25",
         Period_day == "6 Rev")
  # ungroup() %>% 
  # group_by(Experiment, Period_day, IdLabel) %>% 
  # mutate(max_each = n()) %>% 
  # summarise(nrmse = get_gof(mean_sine_choice, sine_vol, nrmse, 1:10)
  #        # rae = get_gof(mean_sine_choice, sine_vol, rae, 10),
  #        # mape = get_gof(mean_sine_choice, sine_vol, mape, 10)
  #        )

check_gof <- get_gof(check$mean_sine_choice, check$sine_vol, 
                rmse, lag_n = 1:72)
check_gof %>% 
  ggplot(aes(lag_n, value)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1, 72, by = 1)) + 
  theme_bw()

# range of lag values
check_2_0.75 <- fivemin_bins_averages %>%
  filter(Period == 2700) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  summarise(
            get_best_gof(mean_sine_choice, sine_vol, mape, lag_n = 0:9), 
            get_best_gof(mean_sine_choice, sine_vol, rae, lag_n = 0:9),
            get_best_gof(mean_sine_choice, sine_vol, nrmse, lag_n = 0:9), 
            get_best_gof(mean_sine_choice, sine_vol, rmse, lag_n = 0:9)
            )

check_2_1.5 <- fivemin_bins_averages %>%
  filter(Period == 5400) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 18), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 18),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 18), 
    get_best_gof(mean_sine_choice, sine_vol, rmse, 18)
  )

check_2_3 <- fivemin_bins_averages %>%
  filter(Period == 10800) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 36), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 36),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 36), 
    get_best_gof(mean_sine_choice, sine_vol, rmse, 36)
  )

check_2_6 <- fivemin_bins_averages %>%
  filter(Period == 21600) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 72), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 72),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 72), 
    get_best_gof(mean_sine_choice, sine_vol, rmse, 72)
  )

fivemin_bin_averages_measures <- bind_rows(check_2_0.75, check_2_1.5, check_2_3, check_2_6)
check_2 <- bind_rows(check_2_0.75, check_2_1.5, check_2_3, check_2_6)
rm(check_2_0.75, check_2_1.5, check_2_3, check_2_6)

fivemin_bin_averages_measures <- fivemin_bin_averages_measures %>%
  mutate(rae = round(rae, digits = 2),
         nrmse = round(nrmse, digits = 2),
         mape = round(mape, digits = 2), 
         rmse = round(rmse, digits = 2))

# plotting it out 
fivemin_bin_averages_measures %>% 
  ggplot() + 
  geom_jitter(aes(rae_lag_n, rae, group = IdLabel, colour = IdLabel)) + 
  xlab("Lag number") + 
  ylab("RAE value") + 
  ylim(0, 10) + 
  facet_grid(Experiment~Period_day) + 
  theme_bw() + 
  theme(legend.position = "none")

fivemin_bin_averages_measures %>% 
  ggplot() + 
  geom_jitter(aes(nrmse_lag_n, nrmse, group = IdLabel, colour = IdLabel)) + 
  facet_grid(Period_day~Experiment) + 
  xlab("Lag number") + 
  ylab("NRMSE value") +
  #xlim(0, 150) + 
  theme_bw() + 
  theme(legend.position = "none")

fivemin_bin_averages_measures %>% 
  ggplot() + 
  geom_jitter(aes(mape_lag_n, mape, group = IdLabel, colour = IdLabel)) + 
  facet_grid(Period_day~Experiment) + 
  xlab("Lag number") + 
  ylab("MAPE value") +
  #xlim(0, 150) + 
  theme_bw() + 
  theme(legend.position = "none")

fivemin_bins_averages <- fivemin_bins_averages %>%
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  mutate(bin_count = n())

# checking the robustness of these measures 

#####
# sanity check 1 - taking different proportions of the data 
#####

# taking samples of the data and calculating the rae and mape and plotting it out

# picking the outliers

outliers <- fivemin_bins_averages %>% 
  ungroup() 
  #filter(bin_count < 70 | bin_count > 120)

# slice sampling the outliers one by one 

outliers_0.5 <- outliers %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.5) %>% 
  mutate(prop = 0.5)

outliers_0.25 <- outliers %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.25) %>% 
  mutate(prop = 0.25)

outliers_0.75 <- outliers %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.75) %>% 
  mutate(prop = 0.75)

outliers <- bind_rows(outliers_0.25, outliers_0.5, outliers_0.75)

rm(outliers_0.25, outliers_0.5, outliers_0.75)

# calculating the rae for each of the samples 

outliers_0.75 <- outliers %>% 
  filter(Period == 2700) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, bin_count, prop) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 1:9), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 1:9),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 1:9)
  ) %>%
  mutate(rae = round(rae, digits = 2),
         nrmse = round(nrmse, digits = 2),
         mape = round(mape, digits = 2))

# and so on 
outliers_1.5 <- outliers %>% 
  filter(Period == 5400) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, bin_count, prop) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 1:18), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 1:18),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 1:18)
  ) %>%
  mutate(rae = round(rae, digits = 2),
         nrmse = round(nrmse, digits = 2),
         mape = round(mape, digits = 2))
  

# plotting out the rae vs sample size for each outlier bat 

outliers_0.75 %>% 
  mutate(prop = as.factor(prop), 
         outlier_type = ifelse(bin_count < 95, "low count", "high count")) %>% 
  filter(outlier_type == "low count") %>% 
  ggplot() + 
  geom_point(aes(prop, nrmse, group = Period_day, colour = Period_day), alpha = 0.5) + 
  facet_wrap(.~ IdLabel)+ 
  ylim(0,20) + 
  xlab("Proportion of data used to calculate NRMSE") + 
  ylab("NRMSE") + 
  theme_bw()

outliers_0.75 %>% 
  mutate(prop = as.factor(prop), 
         outlier_type = ifelse(bin_count < 95, "low count", "high count")) %>% 
  filter(outlier_type == "high count") %>% 
  ggplot() + 
  geom_point(aes(prop, nrmse, group = Period_day, colour = Period_day), alpha = 0.5) + 
  xlab("Proportion of data used to calculate NRMSE") + 
  ylab("NRMSE") + 
  ylim(0, 20) + 
  facet_wrap(.~ IdLabel)+ 
  theme_bw()

######
# Sanity check 2 - repeated simulations with a single proportion 
######

# selecting the bat with the most bins 
most_bins_0.1 <- fivemin_bins_averages %>% 
  filter(bin_count == 144) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.1) %>% 
  mutate(prop = 0.1)

most_bins_0.25 <- fivemin_bins_averages %>% 
  filter(bin_count == 144) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.25) %>% 
  mutate(prop = 0.25)

most_bins_0.5 <- fivemin_bins_averages %>% 
  filter(bin_count == 144) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.5) %>% 
  mutate(prop = 0.5)

most_bins_0.75 <- fivemin_bins_averages %>% 
  filter(bin_count == 144) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.75) %>% 
  mutate(prop = 0.75)

most_bins_0.9 <- fivemin_bins_averages %>% 
  filter(bin_count == 144) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.9) %>% 
  mutate(prop = 0.9)

most_bins <- bind_rows(most_bins_0.1, most_bins_0.25, most_bins_0.5, most_bins_0.75, most_bins_0.9)  
 
rm(most_bins_0.1, most_bins_0.25, most_bins_0.5, most_bins_0.75, most_bins_0.9) 

most_bins <- most_bins %>% 
  #filter(Period == 2700) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, bin_count, prop) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 1:72), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 1:72),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 1:72)
  ) %>%
  mutate(rae = round(rae, digits = 2),
         nrmse = round(nrmse, digits = 2),
         mape = round(mape, digits = 2))

most_bins %>% 
  ggplot() + 
  geom_point(aes(prop, rae, group = Period_day, colour = Period_day)) + 
  #facet_grid(.~) + 
  theme_bw()

# selecting the bat with the fewest bins 

# selecting a bat with a middle value of bins

middle_bins_0.1 <- fivemin_bins_averages %>% 
  filter(bin_count == 100) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.1) %>% 
  mutate(prop = 0.1)

middle_bins_0.25 <- fivemin_bins_averages %>% 
  filter(bin_count == 100) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.25) %>% 
  mutate(prop = 0.25)

middle_bins_0.5 <- fivemin_bins_averages %>% 
  filter(bin_count == 100) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.5) %>% 
  mutate(prop = 0.5)

middle_bins_0.75 <- fivemin_bins_averages %>% 
  filter(bin_count == 100) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.75) %>% 
  mutate(prop = 0.75)

middle_bins_0.9 <- fivemin_bins_averages %>% 
  filter(bin_count == 100) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  slice_sample(prop = 0.9) %>% 
  mutate(prop = 0.9)

middle_bins <- bind_rows(middle_bins_0.1, middle_bins_0.25, middle_bins_0.5, middle_bins_0.75, middle_bins_0.9)  

middle_bins <- middle_bins %>% 
  #filter(Period == 2700) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, bin_count, prop) %>% 
  summarise(
    get_best_gof(mean_sine_choice, sine_vol, mape, 1:72), 
    get_best_gof(mean_sine_choice, sine_vol, rae, 1:72),
    get_best_gof(mean_sine_choice, sine_vol, nrmse, 1:72)
  ) %>%
  mutate(rae = round(rae, digits = 2),
         nrmse = round(nrmse, digits = 2),
         mape = round(mape, digits = 2))

middle_bins %>% 
  ggplot() + 
  geom_point(aes(prop, rae, group = Period_day, colour = Period_day)) + 
  facet_grid(.~IdLabel) + 
  theme_bw()
####
# sanity check - in the existing data is the RAE/NRMSE affected by the size of the dataset
#####
lookup <- fivemin_bins_averages %>% 
  ungroup() %>% 
  select(Experiment, Period_day, IdLabel, bin_count) %>% 
  distinct()

check_2 <- left_join(check_2, lookup, by = c("Experiment", "Period_day", "IdLabel"))

check_2 <- check_2 %>% 
  mutate(bin_count = bin_count/144)

check_2 %>% 
  ggplot() + 
  geom_jitter(aes(bin_count, rae, colour = IdLabel, group = IdLabel)) +
  theme_bw() + 
  #ylim(0,50) + 
  xlab("Bins - prop of max") + 
  ylab("RAE") + 
  facet_wrap(.~Period_day)

check_2 %>% 
  ggplot() + 
  geom_jitter(aes(bin_count, nrmse, colour = IdLabel, group = IdLabel)) +
  theme_bw() + 
  #ylim(0,50) + 
  xlab("Bins - prop of max") + 
  ylab("NRMSE") + 
  facet_wrap(.~Period_day)

check_2 %>% 
  ggplot() + 
  geom_jitter(aes(bin_count, rmse, colour = IdLabel, group = IdLabel)) +
  theme_bw() + 
  #ylim(0,50) + 
  xlab("Bins - prop of max") + 
  ylab("RMSE") + 
  facet_wrap(.~Period_day)


#######
# Which measure is more sensitive - NRMSE or RAE? 
#######

sensitivity <- fivemin_bin_averages_measures %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  summarise(mean_nrmse = mean(nrmse), 
            sd_nrmse = sd(nrmse), 
            mean_rae = mean(rae), 
            sd_rae = sd(rae)) 
  # pivot_longer(cols = c(3:6), names_to = "measures", values_to = "values") %>% 
  # mutate(measure_type = ifelse(str_detect(measures, "nrmse"), "NRMSE", "RAE"), 
  #        mean_sd = ifelse(str_detect(measures, "mean"), "Mean", "SD"))

sensitivity %>% 
  ggplot() + 
  geom_point(aes(IdLabel, mean_nrmse)) +  
  geom_errorbar(aes(x = IdLabel, ymin = mean_nrmse - sd_nrmse, ymax = mean_nrmse + sd_nrmse), 
                width=.2, position=position_dodge(.9)) +
  ylim(-40, 101) + 
  theme_bw() + 
  facet_grid(.~Experiment, scales = "free")

sensitivity %>% 
  ggplot() + 
  geom_point(aes(IdLabel, mean_rae)) +  
  geom_errorbar(aes(x = IdLabel, ymin = mean_rae - sd_rae, ymax = mean_rae + sd_rae), 
                width=.2, position=position_dodge(.9)) +
  ylim(-40, 101) + 
  theme_bw() + 
  facet_grid(.~Experiment, scales = "free")

# the spread is greater for RAE so it seems to be more sensitive to Period 

#####
# What's the cutoff? 
#####

fivemin_bins_averages <- fivemin_bins_averages %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>% 
  mutate(overall_mean = mean(mean_sine_choice))

lookup <- fivemin_bins_averages %>% 
  select(Experiment, Period_day, IdLabel, overall_mean) %>%
  distinct() %>% 
  mutate(overall_mean = round(overall_mean, digits = 2))

fivemin_bin_averages_measures <- left_join(fivemin_bin_averages_measures, lookup, by = c("Experiment", "Period_day", "IdLabel")) 

fivemin_bin_averages_measures %>% 
  ggplot() + 
  geom_point(aes(overall_mean, rae, group = IdLabel, colour = IdLabel)) + 
  facet_grid(Period_day~Experiment) + 
  ylim(0, 10) +
  #scale_y_continuous(breaks = seq(0, 200, by = 10)) + 
  geom_vline(xintercept = c(0.1, 0.90), linetype = "dashed") + 
  geom_vline(xintercept = c(0.05, 0.95), linetype = "dotted") + 
  geom_vline(xintercept = c(0.25, 0.75), linetype = "dotted") +
  theme_bw() + theme(legend.position = "none")

######
# OLD CODE - NOT EXECUTED - different comparisons - matching, maximizing, side preference, fluctuating preference
# First NRMSE 
#######

# creating a lookup table to see which side was preferred overall over all the nights 
side_pref_lookup <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  mutate(flower_num = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
  select(Experiment, IdLabel, flower_num) %>% 
  mutate(side = ifelse(flower_num %% 2 == 1, 1, 0)) %>%
  summarise(pref_odd = mean(side)) %>% 
  mutate(preferred_side = ifelse(pref_odd > 0.5, "odd", "even"))

# creating a lookup table to see which side the fixed and fluctuating outputs occurred 
# on each day
side_output_lookup <- Main_all %>% 
  filter(outFuncLabel == "sineRewOut" | outFuncLabel == "fixRewOut") %>% 
  mutate(output = ifelse(outFuncLabel == "sineRewOut", "fluc", "fix"), 
         side = ifelse((as.numeric(str_extract(unitLabel, "[0-9]+"))) %% 2 == 1, "odd", "even")) %>% 
  select(Experiment, Period_day, IdLabel, output, side) %>% 
  distinct()

# now putting the information together to see whether the fluctuating option occurred 
# on the preferred side or not
side_output_pref_lookup <- left_join(side_output_lookup, side_pref_lookup, by = c("Experiment", "IdLabel")) %>% 
  select(-pref_odd) %>% 
  # here's the problem. On this one night Bat 87 made exclusive visits to its overall 
  # non-preferred side and so it gets filtered out in the next line. 
  # Presumably it's the only animal that did something that bone-headed because there are 
  # no other NaNs or NAs
  filter(side == preferred_side)

# this is a very inelegant solution so it can be fixed, but for now we are just adding
# an extra line to the table with the missing data 

missing_bat <- tibble(Experiment ="Subjective", Period_day = as.character(3), 
                      IdLabel = "Bat 87", output = "fix", side = "even", preferred_side = "odd")

side_output_pref_lookup <- bind_rows(side_output_pref_lookup, missing_bat)

rm(missing_bat)

fivemin_bins_averages <- left_join(fivemin_bins_averages, side_output_pref_lookup, by = c("Experiment", "Period_day", "IdLabel")) %>% 
  # is the fluctuating option occurring on the preferred side? 
  mutate(congruence = case_when(side == "odd" & preferred_side == "odd" ~ 1, 
                                        side == "even" & preferred_side == "even" ~ 1, 
                                        side == "odd" & preferred_side == "even" ~ 0, 
                                        side == "even" & preferred_side == "odd" ~ 0)) %>% 
  select(-side, -preferred_side) %>% 
  # if congruence is 1, then the output is occurring on the preferred side 
  mutate(side_pref_strategy = case_when(output == "fix" & congruence == 1 ~ 0, 
                                        output == "fix" & congruence == 0 ~ 1, 
                                        output == "fluc" & congruence == 1 ~ 1, 
                                        output == "fluc" & congruence == 0 ~ 0))

comparisons <- fivemin_bins_averages %>% 
  mutate(only_fluc_strategy = 1, 
         only_fix_strategy = 0) %>% 
  rename(matching_strategy = sine_vol, 
         maximizing_strategy = ideal_bat)

folder <- "/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/"
write.csv2(comparisons, file = paste0(folder, "analysis/R/strategies.csv"), row.names = FALSE)

write.csv2(fivemin_bins_averages, file = paste0(folder, "analysis/R/fivemin_bins_averages.csv"), row.names = FALSE)

comparisons_0.75 <- comparisons %>%
  filter(Period == 2700) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, 1:9))

comparisons_1.5 <- comparisons %>%
  filter(Period == 5400) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, 1:18))

comparisons_3 <- comparisons %>%
  filter(Period == 10800) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, 1:36))

comparisons_6 <- comparisons %>%
  filter(Period == 21600) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, 1:72))
  

# comparisons_summ <- bind_rows(comparisons_0.75, comparisons_1.5, comparisons_3, comparisons_6) %>%
#   select(Experiment, Period_day, IdLabel, rae_max, rae_match, rae_fluc, rae_side) %>% 
#   distinct() %>% 
#   pivot_longer(cols = c(4:7), names_to = "rae type", values_to = "values") %>% 
#   ungroup() %>% 
#   group_by(Experiment, Period_day, IdLabel) %>% 
#   mutate(minimum = case_when(values == min(values) ~ "minimum", 
#                              TRUE ~ "not minimum"))

comparisons_summ <- bind_rows(comparisons_0.75, comparisons_1.5, comparisons_3, comparisons_6) %>%
  select(Experiment, Period_day, IdLabel, strategy, nrmse, nrmse_lag_n) %>%
  mutate(nrmse = round(nrmse, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  # ungroup() %>% 
  # group_by(Experiment, Period) %>% 
  # mutate(mean_lag = mean(nrmse_lag_n))
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -nrmse_lag_n) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "nrmse") %>% 
  mutate(nrmse = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`)) %>% 
  #pivot_longer(cols = c(4:7), names_to = "nrmse type", values_to = "values") %>%
  ungroup() %>%
  group_by(Experiment, Period, IdLabel) %>%
  mutate(minimum = case_when(nrmse == min(nrmse) ~ "minimum",
                               TRUE ~ "not"))

# check <- comparisons_summ %>% 
#   filter(IdLabel == "Bat 87" | IdLabel == "Bat 77") %>% 
#   ungroup() %>% 
#   mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
#   select(-Period_day, -nrmse_lag_n) %>% 
#   pivot_wider(names_from = "day_treatment", values_from = "nrmse") %>% 
#   mutate(nrmse = `Day 1` + `Day 2`) %>% 
#   select(-c(`Day 1`, `Day 2`))

check <- comparisons_summ %>% 
  ungroup() %>% 
  filter(Experiment == "Subjective", 
         Period == 0.75) %>% 
  select(IdLabel) %>% 
  distinct()

minimum <- comparisons_summ %>%
  filter(minimum == "minimum") %>%
  select(-minimum) %>%
  #rename(minimum_nrmse = `nrmse type`) %>% 
  ungroup() %>% 
  group_by(Experiment, Period, strategy) %>% 
  summarise(count_nrmse_type = n())

minimum_lookup <- comparisons_summ %>% 
  ungroup() %>% 
  select(Experiment, Period, strategy) %>% 
  distinct()

minimum <- left_join(minimum_lookup, minimum, by = c("Experiment", "Period", "strategy")) %>% 
  mutate(count_nrmse_type = ifelse(is.na(count_nrmse_type), 0, count_nrmse_type))

# minimum %>% 
#   mutate(minimum_nrmse = case_when(minimum_nrmse == "nrmse_fluc" ~ "fluc", 
#                                  minimum_nrmse == "nrmse_fix" ~ "fix", 
#                                  minimum_nrmse == "nrmse_max" ~ "max", 
#                                  minimum_nrmse == "nrmse_match" ~ "match")) %>% 
#   ggplot() + 
#   geom_point(aes(minimum_nrmse, count_nrmse_type)) + 
#   facet_grid(Experiment~Period_day) + 
#   xlab("Predicted values that observations were matched to") + 
#   ylab("Number of bats with minimum \n nrmse for the predicted value") + 
#   theme_bw()

minimum %>% 
  mutate(#Period = Period/3600, 
         Period = as.factor(Period), 
         strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                                 strategy == "matching_strategy" ~ "Matching to wave", 
                                 strategy == "maximizing_strategy" ~ "Reward maximizing", 
                                 strategy == "side_pref_strategy" ~ "Side preference", 
                                 strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>%
  ggplot() + 
  geom_col(aes(Period, count_nrmse_type, fill = Strategy, group = Strategy)) + 
  facet_grid(.~Experiment) + 
  scale_y_continuous(breaks = seq(0, 18, by = 2)) + 
  xlab("Period") + 
  ylab("Number of bats using each strategy \n (min NRMSE values)") + 
  theme_bw()

comparisons_summ %>% 
  mutate(#Period = Period*3600, 
         Period = as.factor(Period), 
         strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                              strategy == "matching_strategy" ~ "Matching to wave", 
                              strategy == "maximizing_strategy" ~ "Reward maximizing", 
                              strategy == "side_pref_strategy" ~ "Side preference", 
                              strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>% 
  ggplot() + 
  geom_point(aes(Strategy, nrmse, group = IdLabel, colour = IdLabel), alpha = 0.5, size = 0.5) + 
  geom_line(aes(Strategy, nrmse, group = IdLabel, colour = IdLabel), alpha = 0.5) + 
  facet_grid(Period~Experiment) + 
  ylab("NRMSE of individual bats") + 
  #ylim(0,10) +  
  theme_bw() + 
  theme(legend.position = "none")

######
# OLD CODE - NOT EXECUTED - different comparisons - matching, maximizing, side preference, fluctuating preference
# Now RAE
######

comparisons_0.75 <- comparisons %>%
  filter(Period == 2700) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, rae, 1:9))

comparisons_1.5 <- comparisons %>%
  filter(Period == 5400) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, rae, 1:18))

comparisons_3 <- comparisons %>%
  filter(Period == 10800) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, rae, 1:36))

comparisons_6 <- comparisons %>%
  filter(Period == 21600) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, rae, 1:72))

# comparisons_summ <- bind_rows(comparisons_0.75, comparisons_1.5, comparisons_3, comparisons_6) %>%
#   select(Experiment, Period_day, IdLabel, rae_max, rae_match, rae_fluc, rae_side) %>% 
#   distinct() %>% 
#   pivot_longer(cols = c(4:7), names_to = "rae type", values_to = "values") %>% 
#   ungroup() %>% 
#   group_by(Experiment, Period_day, IdLabel) %>% 
#   mutate(minimum = case_when(values == min(values) ~ "minimum", 
#                              TRUE ~ "not minimum"))

comparisons_summ <- bind_rows(comparisons_0.75, comparisons_1.5, comparisons_3, comparisons_6) %>%
  select(Experiment, Period_day, IdLabel, strategy, rae, rae_lag_n) %>%
  mutate(rae = round(rae, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  # ungroup() %>% 
  # group_by(Experiment, Period) %>% 
  # mutate(mean_lag = mean(rae_lag_n))
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -rae_lag_n) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "rae") %>% 
  mutate(rae = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`)) %>% 
  #pivot_longer(cols = c(4:7), names_to = "rae type", values_to = "values") %>%
  ungroup() %>%
  group_by(Experiment, Period, IdLabel) %>%
  mutate(minimum = case_when(rae == min(rae) ~ "minimum",
                             TRUE ~ "not"))

# check <- comparisons_summ %>% 
#   filter(IdLabel == "Bat 87" | IdLabel == "Bat 77") %>% 
#   ungroup() %>% 
#   mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
#   select(-Period_day, -rae_lag_n) %>% 
#   pivot_wider(names_from = "day_treatment", values_from = "rae") %>% 
#   mutate(rae = `Day 1` + `Day 2`) %>% 
#   select(-c(`Day 1`, `Day 2`))

check <- comparisons_summ %>% 
  ungroup() %>% 
  filter(Experiment == "Objective", 
         Period == 0.75) %>% 
  select(IdLabel) %>% 
  distinct()

minimum <- comparisons_summ %>%
  filter(minimum == "minimum") %>%
  select(-minimum) %>%
  #rename(minimum_rae = `rae type`) %>% 
  ungroup() %>% 
  group_by(Experiment, Period, strategy) %>% 
  summarise(count_rae_type = n())

minimum_lookup <- comparisons_summ %>% 
  ungroup() %>% 
  select(Experiment, Period, strategy) %>% 
  distinct()

minimum <- left_join(minimum_lookup, minimum, by = c("Experiment", "Period", "strategy")) %>% 
  mutate(count_rae_type = ifelse(is.na(count_rae_type), 0, count_rae_type))

# minimum %>% 
#   mutate(minimum_rae = case_when(minimum_rae == "rae_fluc" ~ "fluc", 
#                                  minimum_rae == "rae_fix" ~ "fix", 
#                                  minimum_rae == "rae_max" ~ "max", 
#                                  minimum_rae == "rae_match" ~ "match")) %>% 
#   ggplot() + 
#   geom_point(aes(minimum_rae, count_rae_type)) + 
#   facet_grid(Experiment~Period_day) + 
#   xlab("Predicted values that observations were matched to") + 
#   ylab("Number of bats with minimum \n rae for the predicted value") + 
#   theme_bw()

minimum %>% 
  mutate(#Period = Period/3600, 
    Period = as.factor(Period), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>%
  ggplot() + 
  geom_col(aes(Period, count_rae_type, fill = Strategy, group = Strategy)) + 
  facet_grid(.~Experiment) + 
  scale_y_continuous(breaks = seq(0, 25, by = 2)) + 
  xlab("Period") + 
  ylab("Number of bats using each strategy \n (min RAE values)") + 
  theme_bw()

comparisons_summ %>% 
  mutate(#Period = Period*3600, 
    Period = as.factor(Period), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>% 
  ggplot() + 
  geom_point(aes(Strategy, rae, group = IdLabel, colour = IdLabel), alpha = 0.5, size = 0.5) + 
  geom_line(aes(Strategy, rae, group = IdLabel, colour = IdLabel), alpha = 0.5) + 
  facet_grid(Period~Experiment) + 
  ylab("RAE of individual bats") + 
  #ylim(0,10) +  
  theme_bw() + 
  theme(legend.position = "none")

# Not convinced by RAE. Why does Bat 46 for example score 0 on all the strategies 
# for Period 6??

########
# different strategies - matching, maximizing, side pref, fix pref, fluc pref - NRMSE
########
#######
# old version 
# creating a lookup table to see which side was preferred overall over all the nights 
side_pref_lookup <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  mutate(flower_num = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
  select(Experiment, IdLabel, flower_num) %>% 
  mutate(side = ifelse(flower_num %% 2 == 1, 1, 0)) %>%
  summarise(pref_odd = mean(side)) %>% 
  mutate(preferred_side = ifelse(pref_odd > 0.5, "odd", "even"))

# creating a lookup table to see which side the fixed and fluctuating outputs occurred 
# on each day
side_output_lookup <- Main_all %>% 
  filter(outFuncLabel == "sineRewOut" | outFuncLabel == "fixRewOut") %>% 
  mutate(output = ifelse(outFuncLabel == "sineRewOut", "fluc", "fix"), 
         side = ifelse((as.numeric(str_extract(unitLabel, "[0-9]+"))) %% 2 == 1, "odd", "even")) %>% 
  select(Experiment, Period_day, IdLabel, output, side) %>% 
  distinct()

# now putting the information together to see whether the fluctuating option occurred 
# on the preferred side or not
side_output_pref_lookup <- left_join(side_output_lookup, side_pref_lookup, by = c("Experiment", "IdLabel")) %>% 
  select(-pref_odd) %>% 
  # here's the problem. On this one night Bat 87 made exclusive visits to its overall 
  # non-preferred side and so it gets filtered out in the next line. 
  # Presumably it's the only animal that did something that bone-headed because there are 
  # no other NaNs or NAs
  filter(side == preferred_side)

# this is a very inelegant solution so it can be fixed, but for now we are just adding
# an extra line to the table with the missing data 

missing_bat <- tibble(Experiment ="Subjective", Period_day = as.character(3), 
                      IdLabel = "Bat 87", output = "fix", side = "even", preferred_side = "odd")

side_output_pref_lookup <- bind_rows(side_output_pref_lookup, missing_bat)

rm(missing_bat)

#######
# new version with the cumulative side preference

# creating a lookup table to see which side was preferred overall over all the nights 
side_pref_lookup <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, Day, Period_day, IdLabel) %>% 
  mutate(flower_num = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
  select(Experiment, Day, Period_day, IdLabel, flower_num) %>% 
  mutate(side = ifelse(flower_num %% 2 == 1, 1, 0)) %>%
  summarise(pref_odd = mean(side)) %>% 
  arrange(Experiment, IdLabel, Day) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  # calculating the cumulative side preference over the days 
  mutate(cum_pref_odd = cumsum(pref_odd), 
         day_n = 1:n(), 
         cum_pref_odd = cum_pref_odd/day_n, 
         preferred_side = ifelse(cum_pref_odd > 0.5, "odd", "even")) %>%
  select(Experiment, Period_day, IdLabel, preferred_side)

# creating a lookup table to see which side the fixed and fluctuating outputs occurred 
# on each day
side_output_lookup <- Main_all %>% 
  filter(outFuncLabel == "sineRewOut" | outFuncLabel == "fixRewOut") %>% 
  mutate(output = ifelse(outFuncLabel == "sineRewOut", "fluc", "fix"), 
         side = ifelse((as.numeric(str_extract(unitLabel, "[0-9]+"))) %% 2 == 1, "odd", "even")) %>% 
  select(Experiment, Period_day, IdLabel, output, side) %>% 
  distinct()

# now putting the information together to see whether the fluctuating option occurred 
# on the preferred side or not
side_output_pref_lookup <- left_join(side_output_lookup, side_pref_lookup, by = c("Experiment", "Period_day", "IdLabel")) %>% 
  #select(-pref_odd) %>% 
  # here's the problem. On this one night Bat 87 made exclusive visits to its overall 
  # non-preferred side and so it gets filtered out in the next line. 
  # Presumably it's the only animal that did something that bone-headed because there are 
  # no other NaNs or NAs
  filter(side == preferred_side)


fivemin_bins_averages <- 
  left_join(fivemin_bins_averages, side_output_pref_lookup, by = c("Experiment", "Period_day", "IdLabel")) %>% 
  # is the fluctuating option occurring on the preferred side? 
  mutate(congruence = case_when(side == "odd" & preferred_side == "odd" ~ 1, 
                                side == "even" & preferred_side == "even" ~ 1, 
                                side == "odd" & preferred_side == "even" ~ 0, 
                                side == "even" & preferred_side == "odd" ~ 0)) %>% 
  select(-side, -preferred_side) %>% 
  # if congruence is 1, then the output is occurring on the preferred side 
  mutate(side_pref_strategy = case_when(output == "fix" & congruence == 1 ~ 0, 
                                        output == "fix" & congruence == 0 ~ 1, 
                                        output == "fluc" & congruence == 1 ~ 1, 
                                        output == "fluc" & congruence == 0 ~ 0))

comparisons_nrmse <- fivemin_bins_averages %>% 
  mutate(only_fluc_strategy = 1) %>% 
  rename(matching_strategy = sine_vol, 
         maximizing_strategy = ideal_bat) %>%
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  mutate(max_lag = case_when(strategy == "matching_strategy" ~ 9,
                         strategy == "maximizing_strategy" ~9,
                         TRUE ~ 0)) %>%
  # filter(max_lag == 9) %>%
  # filter(str_detect(strategy, "ma")) %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, lag_n = 0:max(max_lag)))

comparisons_summ_nrmse <- comparisons_nrmse %>%
  select(Experiment, Period_day, IdLabel, strategy, nrmse, nrmse_lag_n) %>%
  mutate(nrmse = round(nrmse, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -nrmse_lag_n) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "nrmse") %>% 
  mutate(nrmse = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`)) %>% 
  #pivot_longer(cols = c(4:7), names_to = "nrmse type", values_to = "values") %>%
  ungroup() %>%
  group_by(Experiment, Period, IdLabel) %>%
  mutate(minimum = case_when(nrmse == min(nrmse) ~ "minimum",
                             TRUE ~ "not"))

check <- comparisons_summ_nrmse %>% 
  ungroup() %>% 
  filter(Experiment == "Subjective", 
         Period == 0.75) %>% 
  select(IdLabel) %>% 
  distinct()

minimum <- comparisons_summ_nrmse %>%
  filter(minimum == "minimum") %>%
  select(-minimum) %>%
  ungroup() %>% 
  group_by(Experiment, Period, strategy) %>% 
  summarise(count_nrmse_type = n())

# creating a lookup table so a 0 can be inserted for those strategies that didn't have 
# any bats doing them on any of the days
minimum_lookup <- comparisons_summ_nrmse %>% 
  ungroup() %>% 
  select(Experiment, Period, strategy) %>% 
  distinct()

minimum <- left_join(minimum_lookup, minimum, by = c("Experiment", "Period", "strategy")) %>% 
  mutate(count_nrmse_type = ifelse(is.na(count_nrmse_type), 0, count_nrmse_type))

minimum %>% 
  mutate(#Period = Period/3600, 
    Period = as.factor(Period), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>%
  ggplot() + 
  geom_col(aes(Period, count_nrmse_type, fill = Strategy, group = Strategy)) + 
  facet_grid(.~Experiment) + 
  scale_y_continuous(breaks = seq(0, 18, by = 2)) + 
  xlab("Period [hours]") + 
  ylab("Number of bats using each strategy \n (min NRMSE values)") + 
  theme_bw()

comparisons_summ_nrmse %>% 
  mutate(#Period = Period*3600, 
    Period = as.factor(Period), 
    nrmse = as.integer(nrmse), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>% 
  ggplot() + 
  geom_point(aes(Strategy, nrmse, group = IdLabel, colour = IdLabel), alpha = 0.5, size = 0.5) + 
  geom_line(aes(Strategy, nrmse, group = IdLabel, colour = IdLabel), alpha = 0.5) + 
  facet_grid(Period~Experiment) + 
  ylab("NRMSE of individual bats") + 
  ylim(0,10) +  
  theme_bw() + 
  theme(legend.position = "none")

######
# different strategies - matching, maximizing, side pref, fix pref, fluc pref - RAE
######
comparisons_rae <- fivemin_bins_averages %>% 
  mutate(only_fluc_strategy = 1) %>% 
  rename(matching_strategy = sine_vol, 
         maximizing_strategy = ideal_bat) %>%
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  mutate(max_lag = case_when(strategy == "matching_strategy" ~ 9,
                             strategy == "maximizing_strategy" ~9,
                             TRUE ~ 0)) %>%
  # filter(max_lag == 9) %>%
  # filter(str_detect(strategy, "ma")) %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, rae, lag_n = 0:max(max_lag)))

comparisons_summ_rae <- comparisons_rae %>%
  select(Experiment, Period_day, IdLabel, strategy, rae, rae_lag_n) %>%
  mutate(rae = round(rae, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -rae_lag_n) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "rae") %>% 
  mutate(rae = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`)) %>% 
  #pivot_longer(cols = c(4:7), names_to = "rae type", values_to = "values") %>%
  ungroup() %>%
  group_by(Experiment, Period, IdLabel) %>%
  mutate(minimum = case_when(rae == min(rae) ~ "minimum",
                             TRUE ~ "not")) %>% 
  arrange(Experiment, IdLabel, Period)

check <- comparisons_summ_rae %>% 
  ungroup() %>% 
  filter(Experiment == "Objective", 
         Period == 0.75) %>% 
  select(IdLabel) %>% 
  distinct()

minimum <- comparisons_summ_rae %>%
  filter(minimum == "minimum") %>%
  select(-minimum) %>%
  ungroup() %>% 
  group_by(Experiment, Period, strategy) %>% 
  summarise(count_rae_type = n())

minimum_lookup <- comparisons_summ_rae %>% 
  ungroup() %>% 
  select(Experiment, Period, strategy) %>% 
  distinct()

minimum <- left_join(minimum_lookup, minimum, by = c("Experiment", "Period", "strategy")) %>% 
  mutate(count_rae_type = ifelse(is.na(count_rae_type), 0, count_rae_type))

minimum %>% 
  mutate(#Period = Period/3600, 
    Period = as.factor(Period), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>%
  ggplot() + 
  geom_col(aes(Period, count_rae_type, fill = Strategy, group = Strategy)) + 
  facet_grid(.~Experiment) + 
  scale_y_continuous(breaks = seq(0, 18, by = 2)) + 
  xlab("Period [hours]") + 
  ylab("Number of bats using each strategy \n (min RAE values)") + 
  theme_bw()

comparisons_summ_rae %>% 
  mutate(#Period = Period*3600, 
    Period = as.factor(Period), 
    strategy = case_when(strategy == "only_fluc_strategy" ~ "Only fluctuating", 
                         strategy == "matching_strategy" ~ "Matching to wave", 
                         strategy == "maximizing_strategy" ~ "Reward maximizing", 
                         strategy == "side_pref_strategy" ~ "Side preference", 
                         strategy == "only_fix_strategy" ~ "Only fixed")) %>% 
  rename(Strategy = strategy) %>% 
  ggplot() + 
  geom_point(aes(Strategy, rae, group = IdLabel, colour = IdLabel), alpha = 0.5, size = 0.5) + 
  geom_line(aes(Strategy, rae, group = IdLabel, colour = IdLabel), alpha = 0.5) + 
  facet_grid(Period~Experiment) + 
  ylab("RAE of individual bats") + 
  ylim(0,10) +  
  theme_bw() + 
  theme(legend.position = "none")

######
# sanity check - lags
#####
#####
# what is the distribution of average lag values across the different periods? 

lag_distribution <- comparisons_maxmatch %>%
  select(Experiment, Period_day, IdLabel, strategy, rae, rae_lag_n) %>%
  mutate(Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  ungroup() %>%
  group_by(Experiment, Period, strategy) %>%
  summarise(mean_lag = mean(rae_lag_n))

lag_distribution %>% 
  mutate(Period = as.factor(Period)) %>% 
  ggplot() + 
  geom_point(aes(Period, mean_lag)) +
  facet_grid(Experiment~strategy) + 
  scale_y_continuous(breaks = seq(0, 9, by = 1)) + 
  theme_bw()

# OK so the lag values seem to increase and it's weird that this is the case  

# bat by bat - the distribution of NRMSE values for the different strategies and lag numbers

# picking the bat 

example_bat <- comparisons %>% 
  filter(IdLabel == "Bat 39")

# calculating the nrmse values for the different strategies with the lags

example_bat_period <- example_bat %>%
  filter(Period == 5400) %>% 
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_gof(mean_sine_choice, prediction, nrmse, 0:9)) 
  #mutate(value = round(value, digits = 5))

example_bat_period %>% 
  filter(strategy == "maximizing_strategy") %>% 
  ggplot(aes(lag_n, value, colour = strategy, group = strategy)) + 
  #geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(1, 9, by = 1)) + 
  #scale_y_continuous(breaks = seq(1, 7, by = 0.5)) + 
  facet_grid(Period_day~., scales = "free") + 
  ylab("NRMSE value") + 
  xlab("Lag number") + 
  theme_bw()
# which is the minimum 
# different periods 

#### adding the calculated lag and seeing if it fits 
singlebat_lagcheck <- fivemin_bins %>% 
  ungroup() %>%
  group_by(Experiment, Tracking, Period_day, Period, IdLabel, time_bins) %>% 
  summarise(mean_sine_choice = mean(sine_choice)) %>% 
  mutate(timediff = 300, 
         timediff = cumsum(timediff), 
         timediff = timediff - 300, 
         timediff = ifelse(timediff != 0, timediff - 150, timediff), 
         Period = Period * 3600) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(sine_vol = 0.5 * sin(2 * pi * (1 / Period) * (timediff + 14*300) + (pi / 2)) + 0.5, 
         fixed_vol = ifelse(Experiment == "Objective", fixed_vol_obj, fixed_vol_subj), 
         ideal_bat = ifelse(sine_vol > fixed_vol, 1, 0))


#######
# create a tracking model: 
#######
#######
# old code, with the multinomial approach
# analysis_tracking <- minimum %>% 
#   mutate(Period = Period/3600, 
#          Period = as.factor(Period), 
#          total_bats = ifelse(Experiment == "Objective", 18, 16))
# 
# b10.6 <-
#   brm(data = d, family = binomial,
#       admit | trials(applications) ~ 1 + male ,
#       prior = c(prior(normal(0, 10), class = Intercept),
#                 prior(normal(0, 10), class = b)),
#       iter = 2500, warmup = 500, cores = 2, chains = 2, thin = 3, 
#       seed = 10)   
# 
# m.tracking <- 
#   # set stronger priors
#   brm(data = analysis_tracking, family = binomial, 
#       count_nrmse_type | trials(total_bats) ~ 0 + Experiment + Period + strategy + 
#         Period:strategy + Experiment: strategy, 
#       control = list(adapt_delta = 0.999,
#                      max_treedepth = 20), 
#       iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 10)


# after discussion with York and Vladi, the new criterion was agreed upon: 
# if the bats had side preference for 2 treatments or more, they couldn't be included

sidepref_rewmax_nrmse <- fivemin_bins_averages %>% 
  rename(matching_strategy = sine_vol, 
         maximizing_strategy = ideal_bat) %>%
  ungroup() %>% 
  pivot_longer(cols = contains("_strategy"), names_to = "strategy", values_to = "prediction") %>% 
  mutate(max_lag = case_when(strategy == "matching_strategy" ~ 9,
                             strategy == "maximizing_strategy" ~9,
                             TRUE ~ 0)) %>%
  filter(strategy == "maximizing_strategy" | strategy == "side_pref_strategy") %>% 
  # filter(max_lag == 9) %>%
  # filter(str_detect(strategy, "ma")) %>% 
  group_by(Experiment, Period_day, IdLabel, strategy) %>% 
  summarise(get_best_gof(mean_sine_choice, prediction, nrmse, lag_n = 0:max(max_lag))) %>%
  ungroup() %>%
  group_by(Experiment, Period_day, IdLabel) %>%
  mutate(minimum = case_when(nrmse == min(nrmse) ~ "minimum",
                             TRUE ~ "not"))
  
sidepref_rewmax_nrmse_summ <- sidepref_rewmax_nrmse %>%
  select(Experiment, Period_day, IdLabel, strategy, nrmse, nrmse_lag_n) %>%
  mutate(nrmse = round(nrmse, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>%
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -nrmse_lag_n) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "nrmse") %>% 
  mutate(nrmse = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`)) %>% 
  #pivot_longer(cols = c(4:7), names_to = "nrmse type", values_to = "values") %>%
  ungroup() %>%
  group_by(Experiment, Period, IdLabel) %>%
  mutate(minimum = case_when(nrmse == min(nrmse) ~ "minimum",
                             TRUE ~ "not"))

# making a look-up table to see which bats to include 
non_side_pref_bats <- sidepref_rewmax_nrmse_summ %>% 
  mutate(side_pref_check = case_when(strategy == "side_pref_strategy" & minimum == "minimum" ~ 1, 
                                     TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  summarise(side_pref_2treatments = sum(side_pref_check)) %>% 
  mutate(include = ifelse(side_pref_2treatments >= 2, "no", "yes"))

# honestly I think the side pref criterion needs to be revised, but the above is the way to do it based on nrmse values
#######
# let's try a new criterion: if a bat was reward maximizing on one night of a treatment 
# it's not a side preference and we exclude bats that had a side preference on 

trial <- sidepref_rewmax_nrmse %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>%
  mutate(minimum = case_when(nrmse == min(nrmse) ~ "minimum",
                             TRUE ~ "not")) %>% 
  filter(minimum == "minimum") %>% 
  mutate(Period = as.numeric(str_extract(Period_day, "[0-9]+")), 
         Period = case_when(Period == 0 ~ 0.75, 
                            Period == 1 ~ 1.5, 
                            TRUE ~ as.numeric(Period)), 
         check = ifelse(strategy == "maximizing_strategy", 1, 0)) %>% 
  ungroup() %>% 
  group_by(Experiment, Period, IdLabel) %>% 
  mutate(check = sum(check),
         period_strategy = case_when(check > 0 ~ "maximizing_strategy", 
                                     check == 0 ~ "side_pref_strategy")) %>% 
  select(Experiment, Period, IdLabel, period_strategy) %>% 
  distinct() %>% 
  rename(strategy = period_strategy) %>% 
  mutate(take_strategy = "yes")

sidepref_rewmax_nrmse_criterion <- sidepref_rewmax_nrmse %>% 
  mutate(Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>% 
  left_join(trial, by = c("Experiment", "Period", "IdLabel", "strategy")) %>% 
  filter(take_strategy == "yes") %>% 
  ungroup() %>%
  mutate(day_treatment = ifelse(str_detect(Period_day, "Rev"), "Day 2", "Day 1")) %>% 
  select(-Period_day, -nrmse_lag_n, -take_strategy) %>% 
  pivot_wider(names_from = "day_treatment", values_from = "nrmse") %>% 
  mutate(nrmse = `Day 1` + `Day 2`) %>% 
  select(-c(`Day 1`, `Day 2`))

# making a look-up table to see which bats to include 
non_side_pref_bats_criterion <- sidepref_rewmax_nrmse_criterion %>% 
  mutate(side_pref_check = ifelse(strategy == "side_pref_strategy", 1, 0)) %>% 
  # mutate(side_pref_check = case_when(strategy == "side_pref_strategy" & minimum == "minimum" ~ 1, 
  #                                    TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  summarise(side_pref_2treatments = sum(side_pref_check)) %>% 
  mutate(include = ifelse(side_pref_2treatments >= 2, "no", "yes")) %>% 
  filter(include == "yes")

###### new criterion - a treatment that has at least 1 night with NRMSE = 0 is excluded;
# a bat that has this criterion fulfilled for two or more treatments should be excluded 

sidepref_rewmax_nrmse_keep <- sidepref_rewmax_nrmse %>%
  select(Experiment, Period_day, IdLabel, strategy, nrmse, nrmse_lag_n) %>%
  mutate(nrmse = round(nrmse, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>% 
  ungroup() %>% 
  mutate(check = ifelse(nrmse == 0, 0, 1)) %>% 
  group_by(Experiment, IdLabel, Period) %>% 
  mutate(check_period = sum(check)) %>% 
  filter(check_period == 4) %>% 
  select(-check, -check_period) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  mutate(check_bats = n()) %>% 
  filter(check_bats > 8)

batlist <- sidepref_rewmax_nrmse_keep %>% select(Experiment , IdLabel) %>% distinct()
