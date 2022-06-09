######
# Reading in packages 
######
rm(list = ls())

library(tidyverse)
library(brms)
library(bayesplot)
library(lubridate)
library(broom)

#######
# Reading in and preparing the data 
#######

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

#######
# Creating the functions to calculate the goodness of fit values
####### 
# plotting an example
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

# creating a data frame to apply the goodness of fit function for NRMSE 

######
# binning into 5 minute bins 
#######
# Binning data from the Objective mean experiment
# setting this for a binsize that is a proportion of the period - this and binsize_min 
# are the only variable that needs to be set for the analyses below:
binratio <- 1/18 
#setting this for a fixed binsize in units minutes
binsize_min <- 5 
#calculating fixed binsize in seconds
binsize_sec <- binsize_min*60 
binsize <- data.frame(Period = c(0.75, 1.5, 3, 6))
binsize <- binsize %>%
  mutate(Period_min = Period*60, 
         prop_binsize = 60*round(Period*60*binratio),
         prop_binnumber = 3600*12/prop_binsize,
         #setting a column for whichever actual fixed binsize is used 
         fixed_binsize = binsize_sec, 
         #setting an equal binsize for all the periods for now
         fixed_binnumber = 3600*12/binsize_sec) 

#Creating a table with the information about periods and days
Datetimes <- Main_all %>% 
  filter(Experiment == "Objective") %>% 
  ungroup() %>%
  select(Day, Period_day, IdLabel, DateTime, Tracking) %>%
  #mutate(day = day(DateTime)) %>%
  select(-DateTime) %>%
  distinct() %>%
  arrange(IdLabel)

# creating helper sequences of the timeseries with the Datetimes of the objective 
# mean experiment
timeseries1 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-19 15:00:00"), to = as.POSIXct("2020-06-27 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries2 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-29 15:00:00"), to = as.POSIXct("2020-07-10 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries3 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-07-16 15:00:00"), to = as.POSIXct("2020-07-24 03:00:00"), by = as.numeric(binsize[1,5])))

timeseries <- bind_rows(timeseries1, timeseries2, timeseries3) %>%  
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  # filtering and removing the non-experimental times
  filter(Hour >= 15 | Hour < 3) %>% 
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
# creating a sequence of the day numbers
seq <- c(2:9, 11:21, 23:30)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

# putting it all together to create one giant lookup table
timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins) %>% 
  filter(!is.na(time_bins)) %>% 
  mutate(time_bins = as.character(time_bins))

# removing the timeseries helper sequences
rm(timeseries1, timeseries2, timeseries3)

# binning the Main data into 5 minute bins 
Main_all <- Main_all %>%
  group_by(Period_day, IdLabel) %>%
  mutate(time_bins = floor_date(as_datetime(DateTime), unit = "5 minute"))

# creating a separate table for the workrate binning
fivemin_bins_obj_intm <- Main_all %>%
  filter(Experiment == "Objective", 
         #Tracking == "tracker"
  ) %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking) %>% 
  mutate(time_bins = as.character(time_bins))

# adding the time series data to the experimental data
fivemin_bins_obj <- left_join(timeseries, fivemin_bins_obj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(time_bins = as.POSIXct(time_bins), 
         Hour = hour(time_bins)) %>%
  filter(Hour < 3 | Hour >= 15) %>%
  select(-Hour) %>% 
  mutate(Experiment = "Objective")

# Repeating the above steps for the Subjective mean experiment

# creating a table with the information about periods and days
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

timeseries <- bind_rows(timeseries_1, timeseries_2, timeseries_3) %>%
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  filter(Hour >= 14 | Hour < 2) %>%
  # filtering and removing the non-experimental times
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
seq <- c(2:9,11:19,21:29)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

# adding the information about the periods to the timeseries and removing NAs
timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins) %>% 
  mutate(time_bins = as.character(time_bins))

# creating a separate table for the workrate binning
fivemin_bins_subj_intm <- Main_all %>%
  filter(Experiment == "Subjective", 
         #Tracking == "tracker"
  ) %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking) %>% 
  mutate(time_bins = as.character(time_bins))

# adding the time series data to the experimental data
fivemin_bins_subj <- left_join(timeseries, fivemin_bins_subj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(time_bins = as.POSIXct(time_bins), 
         Hour = hour(time_bins)) %>%
  filter(Hour < 2 | Hour >= 14) %>%
  select(-Hour) %>%
  mutate(Experiment = "Subjective")

rm(fivemin_bins_subj_intm, fivemin_bins_obj_intm)
# putting it together to create one big data frame with the binned data 
fivemin_bins <- bind_rows(fivemin_bins_subj, fivemin_bins_obj)

# setting the minimum and maximum sine volume 
min_sinevol <- 1.944
max_sinevol <- 24.624

# calculating the fixed volume 
fixed_vol_subj <- (7.128 - min_sinevol)/(max_sinevol - min_sinevol)
fixed_vol_obj <- (13.608 - min_sinevol)/(max_sinevol - min_sinevol)

fivemin_bins <- fivemin_bins %>% 
  filter(!is.na(sine_vol)) %>% 
  # adding a column with 1s and 0s for the choice to the fluctuating volume
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0))

# calculating the average fluctuating choice per bin 
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

#######
# Summary plot for a single bat 
#######

# creating a summary plot for a single bat at a time
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

#######
# Calculating the NRMSE values 
#######

# creating a lookup table to see which side was preferred overall over all the nights
# in a cumulative fashion 

side_pref_lookup <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, Day, Period_day, IdLabel) %>% 
  # creating a column for the flower number
  mutate(flower_num = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
  select(Experiment, Day, Period_day, IdLabel, flower_num) %>% 
  # creating a 'side' column, dividing flower numbers into odd and even
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

# calculating the nrmse values for a matching and side preference strategy
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

# implementing a criterion for which bats to keep: a treatment that has at least 1 night 
# with NRMSE = 0 is excluded; a bat that has this criterion fulfilled for two or more 
# treatments should be excluded
sidepref_rewmax_nrmse_keep <- sidepref_rewmax_nrmse %>%
  select(Experiment, Period_day, IdLabel, strategy, nrmse, nrmse_lag_n) %>%
  mutate(nrmse = round(nrmse, digits = 4), 
         Period = as.numeric(str_extract(Period_day, "[0-9]+")),
         Period = ifelse(Period == 0, 0.75, ifelse(Period == 1, 1.5, Period))) %>% 
  ungroup() %>% 
  # checking whether nrmse values = 0 
  mutate(check = ifelse(nrmse == 0, 0, 1)) %>% 
  group_by(Experiment, IdLabel, Period) %>% 
  # remove those treatments that did not have NRMSE = 0
  mutate(check_period = sum(check)) %>% 
  filter(check_period == 4) %>% 
  select(-check, -check_period) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel) %>% 
  # remove those bats that had two or more treatments removed
  mutate(check_bats = n()) %>% 
  filter(check_bats > 8)

# checking how many bats we have left with a helper table
batlist <- sidepref_rewmax_nrmse_keep %>% select(Experiment, IdLabel) %>% distinct()

#######
# Statistical model of NRMSE
#######

# setting the working directory 
setwd("/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/analysis/R/")

# preparing the data for the models 
nrmse_model <- sidepref_rewmax_nrmse_keep %>% 
  filter(strategy == "maximizing_strategy") %>% 
  ungroup() %>% 
  select(Experiment, Period, IdLabel, nrmse) %>% 
  group_by(Experiment, IdLabel, Period) %>% 
  #summarise(nrmse = sum(nrmse)) %>% 
  mutate(Experiment = ifelse(Experiment == "Objective", "Fix = 13.5", "Fix = 7")) %>%
  rename(Bat = IdLabel, 
         Contrast = Experiment) %>% 
  mutate (Period = as.factor(Period)) %>% 
  # filtering the outliers
  filter(nrmse < 10) 

# plotting the distribution of NRMSE values
nrmse_model %>%  
  #filter(nrmse < 5) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_density(aes(nrmse, group = Period, colour = Period)) + 
  facet_grid(.~Contrast) + 
  xlab("NRMSE values") + 
  ylab("Density")

nrmse_model_check <- nrmse_model %>% 
  ungroup() %>% 
  group_by(Contrast, Period) %>% 
  summarise(avg_nrmse = mean(nrmse), 
            sd_nrmse = sd(nrmse))

# fitting the models 
# MODEL 1: NO INTERACTION BETWEEN PERIOD AND CONTRAST
# m.nrmse <-
#   brm(data = nrmse_model, family = lognormal,
#       nrmse ~ Period + Contrast + (1 + Period|Bat),
#       prior = c(prior(normal(0,0.5), class = Intercept),
#                 prior(normal(0,0.5), class = b),
#                 prior(cauchy(0,1), class = sd)
#       ),
#       #save_pars = save_pars(all = TRUE), 
#       iter = 9000, warmup = 4500, chains = 4, cores = 4, thin = 3,
#       control = list(adapt_delta = 0.9, max_treedepth = 15),
#       seed = 12)
# # 
# save(m.nrmse, file = "m.nrmse.rda")

# MODEL 2: PERIOD:CONTRAST INTERACTION - this is the model that really makes sense 

m.nrmse.interaction <-
  brm(data = nrmse_model, family = lognormal,
      nrmse ~ Period + Contrast + Period:Contrast + (1 + Period|Bat),
      prior = c(prior(normal(0, 0.5), class = Intercept),
                prior(normal(0, 0.5), class = b), 
                prior(cauchy(0, 0.5), class = sd),
                prior(lkj(2), class =cor)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 15),
      seed = 12)

save(m.nrmse.interaction, file = "m.nrmse.interaction.rda")

plot(conditional_effects(m.nrmse.interaction), prob = 0.89)

# comparing the models with LOO 

load("m.nrmse.interaction.rda")

# calculating the WAIC for each model

# checking the model diagnostics

# 1. R^
print(m.nrmse.interaction)

# 2. Chains
post_b <- posterior_samples(m.nrmse.interaction, pars = "^b", add_chain = T)
post_sd <- posterior_samples(m.nrmse.interaction, pars = "^sd", add_chain = T)
post_r <- posterior_samples(m.nrmse.interaction, pars = "^r", add_chain = T)

color_scheme_set("orange")

post_b %>%
  mcmc_trace(facet_args = list(ncol = 4))

post_sd %>%
  mcmc_trace(facet_args = list(ncol = 4))

post_r %>%
  select(contains("Period1.5")) %>% 
  mcmc_trace(facet_args = list(ncol = 4))
  
# 3. neff/N ratios 
  
neff_ratio(m.nrmse.interaction) %>% 
  mcmc_neff() +
  theme_bw()
  
# plotting the regression coefficients 
  mcmc_intervals(m.nrmse.interaction, 
                 pars = vars(1:8),
                 prob_outer = 0.89, 
                 point_size = 1.1
  )
  
# plotting the conditional effects plots
  
int_conditions <- list(Period = c("0.75", "1.5", "3", "6"), Contrast = c("High", "Low"))
  
m_nrmse_interaction <- conditional_effects(m.nrmse.interaction, int_conditions = int_conditions, nsamples = 150, prob = 0.89)
  
# the index value for plot can be changed to anything between 1 and 6
plot(m_nrmse_interaction, plot = FALSE) [[3]] +
    #ylim(0.25,1) + 
    xlab("Period") + 
    ylab("NRMSE value") + 
    #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
    theme_bw()
  
  plot(m_nrmse_interaction, plot = FALSE) [[2]] +
    #ylim(0.25,1) + 
    xlab("Contrast") + 
    ylab("Proportion of visits to \n the fluctuating option") + 
    #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
    theme_bw()
  
  plot(m_nrmse_interaction, plot = FALSE) [[3]] +
    xlab("Period") + 
    ylab("Proportion of visits to \n the fluctuating option") + 
    #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
    theme_bw()
  
#######
# Calculating and plotting the preference for the fluctuating option during the trends
#######

# creating a table with the information from the two trends and periods
Updown <- Main_all %>%
  select(IdLabel, timediff, outFuncLabel, Fixed_steps, sine_steps, 
         sine_vol, reinforce1value, Period, Period_day, Amplitude, Disp, 
         Tracking, Experiment) %>%
  # re-calculating the sine-wave based on what the animal has experienced last instead of the 'actual' wave
  mutate(Period = Period * 3600,
         # converting the timediff column into seconds
         timediff = timediff * 3600,
         # calculating the sine wave values and converting from pump step values to microLitres
         sine_vol = (Amplitude * sin(2 * pi * (1 / Period) * timediff + (pi / 2)) + Disp) * step_conv,
         Period = Period / 3600) %>% 
  #filter(sine_vol < 24 & sine_vol > 2) %>%
  group_by(Experiment, Period, Tracking, IdLabel) %>%
  mutate(
    fix_vol = Fixed_steps * step_conv, 
    # rel_intensity = abs((sine_vol - fix_vol)/((sine_vol + fix_vol)/2)), 
    # rel_intensity = round(rel_intensity, digits = 2)
    ) %>% 
  #filter(rel_intensity > 0.69) %>% 
  mutate(
    trend = ifelse(sine_vol > lag(sine_vol), "up", ifelse(sine_vol < lag(sine_vol), "down", "same")), 
    # accounting for the difference
    outFuncLabel = ifelse(outFuncLabel == "sineRewOut", 1, 0)) %>% 
  filter(!is.na(trend))
  
# plotting it out 
  bats_to_keep <- sidepref_rewmax_nrmse_keep %>% 
    ungroup() %>% 
    select(Experiment, IdLabel) %>% 
    rename(Bat = IdLabel) %>% 
    distinct() %>% 
    mutate(keep = "yes")
  
Updown_plots <- Updown %>% 
  rename(Bat = IdLabel) %>%
  left_join(bats_to_keep, by = c("Experiment", "Bat")) %>% 
  filter(keep == "yes") %>% 
  group_by(Experiment, Period, Bat, trend, sine_vol) %>%
  summarise(pref_fluc = mean(outFuncLabel)) %>% 
  rename(IdLabel = Bat)

Updown_plots_avg <- Updown_plots %>% 
  mutate(sine_vol = round(sine_vol)) %>%
  ungroup() %>% 
  group_by(Experiment, Period, trend, sine_vol) %>%
  mutate(avg_pref_fluc = mean(pref_fluc))

Up <- Updown_plots %>%
  filter(trend == "up")

Down <- Updown_plots %>%
  filter(trend == "down")

Up %>% 
  ggplot() +
  geom_line(aes(sine_vol, pref_fluc, group = IdLabel, colour = trend), stat = "smooth", method = "loess", alpha = 0.3, size = 0.5) +
  geom_line(data = Down, aes(sine_vol, pref_fluc, group = IdLabel, colour = trend), stat = "smooth", method = "loess", alpha = 0.3, size = 0.5) +
  geom_line(data = Updown_plots_avg, aes(sine_vol, pref_fluc, colour = trend), stat = "smooth", method = "loess", se = FALSE) +
  ylim(0,1.2) +
  facet_grid(Experiment ~ Period) +
  xlab ("Volume of the fluctuating option") +
  ylab("Proportion of choices to the fluctuating option") +
  #geom_vline(data = fixedvol, aes(xintercept = fixedvol), linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  theme_bw()

#######
# Statistical model of fluctuating preference in the trends 
#######
# preparing the data for the models 

Trends_model <- Updown %>%
  ungroup() %>% 
  select(Experiment, Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel) %>% 
  filter(trend != "same") %>% 
  # joining with the lookup table of the bats that passed the side-preference criterion
  left_join(bats_to_keep, by = c("Experiment", "Bat")) %>% 
  filter(keep == "yes") %>% 
  select(-keep) %>% 
  rename(Contrast = Experiment) %>%
  mutate(Contrast = ifelse(Contrast == "Objective", "Low", "High"))

# fitting the models 

# MODEL 1: SLOPE OF PERIOD:TREND VOLUME VARYING WITH EACH BAT
# THIS MODEL IS NO LONGER ACCURATE BECAUSE HALF OF THE OBJECTIVE MEAN DATA WERE CUT OUT 
# BECAUSE OF THE RELATIVE INTENSITY CRITERION

# m.trends.somevaryingslopes <-
#   brm(data = Trends_model, family = bernoulli,
#       sine_choice ~ sine_vol + Period + Contrast + trend:sine_vol +
#         Period:trend + (1 + Period:trend|Bat),
#       # should allow intercept to vary by bat
#       prior = c(prior(normal(0, 5), class = Intercept),
#                 prior(normal(0, 5), class = b),
#                 prior(cauchy(0, 1), class = sd)
#       ),
#       iter = 3000, warmup = 1500, chains = 4, cores = 4, thin = 3,
#       control = list(adapt_delta = 0.9, max_treedepth = 12),
#       seed = 12)

save(m.trends.somevaryingslopes, file = "m.trends.somevaryingslopes.rda")

write.csv2(Trends_model, file = "Trends_model.csv", row.names = FALSE)

# MODEL 2: SLOPE OF TREND:FLUCTUATING VOLUME AND PERIOD:TREND VARYING WITH EACH BAT

m.trends.allvaryingslopes <-
  brm(data = Trends_model, family = bernoulli,
      # sine_choice ~ sine_vol + Period + Contrast + trend:sine_vol +
      #   Period:trend + (1 + Period:trend + trend:sine_vol|Bat),
      sine_choice ~ Period:trend:Contrast + 
        (1 + Period:trend + trend:sine_vol|Bat), 
      # should allow intercept to vary by bat
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 2), class = b), 
                prior(cauchy(0, 1), class = sd)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.8, max_treedepth = 12),
      seed = 12)

save(m.trends.allvaryingslopes, file = "m.trends.allvaryingslopes.rda")

load("m.trends.somevaryingslopes.rda")
load("m.trends.allvaryingslopes.rda")

# comparing the models with LOO 

# calculating the WAIC for each model

m.trends.allvaryingslopes <- add_criterion(m.trends.allvaryingslopes, "waic")
m.trends.somevaryingslopes <- add_criterion(m.nrmse.interaction, "waic")

w <- loo_compare(m.trends.allvaryingslopes, m.trends.somevaryingslopes, criterion = "waic") %>% 
  print(simplify = F)

# checking the model diagnostics

# 1. R^
print(m.trends.allvaryingslopes)

# 2. Chains

post_b <- posterior_samples(m.trends.allvaryingslopes, pars = "^b", add_chain = T)
post_sd <- posterior_samples(m.trends.allvaryingslopes, pars = "^sd", add_chain = T)
post_r <- posterior_samples(m.trends.allvaryingslopes, pars = "^r", add_chain = T)

color_scheme_set("orange")

post_b %>%
  mcmc_trace(facet_args = list(ncol = 4))

post_sd %>%
  mcmc_trace(facet_args = list(ncol = 4))

post_r %>%
  select(contains("Intercept"))
mcmc_trace(facet_args = list(ncol = 4))

# 3. neff/N ratios 

neff_ratio(m.trends.allvaryingslopes) %>% 
  mcmc_neff() +
  theme_bw()

# plotting the regression coefficients 
mcmc_intervals(m.trends.allvaryingslopes, 
               pars = vars(1:12),
               prob_outer = 0.89, 
               point_size = 1.1
)
# plotting the conditional effects plots
int_conditions <- list(sine_vol = seq(2, 25, by = 0.23), Period = c("0.75", "1.5", "3", "6"), Contrast = c("High", "Low"))

m_trends_allslopes <- conditional_effects(m.trends.allvaryingslopes, int_conditions = int_conditions, nsamples = 150, prob = 0.89)

# the index value for plot can be changed to anything between 1 and 6
plot(m_trends_allslopes, plot = FALSE) [[1]] +
  ylim(0.25,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  xlab("Fluctuating volume") + 
  ylab("Proportion of visits to \n the fluctuating option") + 
  #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
  theme_bw()

plot(m_trends_allslopes, plot = FALSE) [[2]] +
  ylim(0.25,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  xlab("Period") + 
  ylab("Proportion of visits to \n the fluctuating option") + 
  #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
  theme_bw()

plot(m_trends_allslopes, plot = FALSE) [[3]] +
  ylim(0.25,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  xlab("Contrast") + 
  ylab("Proportion of visits to \n the fluctuating option") + 
  #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
  theme_bw()

plot(m_trends_allslopes, plot = FALSE) [[4]] +
  ylim(0.25,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  xlab("Fluctuating volume") + 
  ylab("Proportion of visits to \n the fluctuating option") + 
  #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
  theme_bw()

plot(m_trends_allslopes, plot = FALSE) [[5]] +
  ylim(0.25,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  xlab("Period") + 
  ylab("Proportion of visits to \n the fluctuating option") + 
  #scale_x_continuous(breaks = seq(1,3,  by = 1)) +  
  theme_bw()
