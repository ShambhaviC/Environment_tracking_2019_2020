#####
# Statistical analyses
#####
rm(list = ls())

#library(season)
library(tidyverse)
library(brms)
library(bayesplot)
library(lubridate)
#library(broom)

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

########
# here we set the minimum noticeable difference. 
# Attempt 1: without any difference

mindiff <- 1

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
    rel_intensity = abs((sine_vol - fix_vol)/((sine_vol + fix_vol)/2)), 
    rel_intensity = round(rel_intensity, digits = 2)) %>% 
  filter(rel_intensity > 0.69) %>% 
  mutate(
    #sine_vol = round(sine_vol),
    # Prof = case_when(outFuncLabel == "sineRewOut" & reinforce1value >= Fixed_steps ~ 1,
    #                  outFuncLabel == "sineRewOut" & reinforce1value < Fixed_steps ~ 0,
    #                  outFuncLabel == "fixRewOut" & reinforce1value >= sine_steps ~ 1,
    #                  outFuncLabel == "fixRewOut" & reinforce1value < sine_steps ~ 0), 
    # one of the following lines can be commented out depending on how 'up' and 'down' are classified
    #trend = ifelse((sine_vol - lag(sine_vol)) > mindiff, "up", ifelse((sine_vol - lag(sine_vol)) < -(mindiff), "down", "same")), 
    trend = ifelse(sine_vol > lag(sine_vol), "up", ifelse(sine_vol < lag(sine_vol), "down", "same")), 
    # accounting for the difference
    outFuncLabel = ifelse(outFuncLabel == "sineRewOut", 1, 0)) %>% 
  filter(!is.na(trend))
# group_by(Experiment, Period, Tracking, IdLabel, trend, sine_vol) %>%
# summarise(pref_fluc = mean(outFuncLabel))

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

Up <- Updown_plots %>%
  filter(trend == "up")

Down <- Updown_plots %>%
  filter(trend == "down")

Up %>% 
    ggplot(aes(sine_vol, pref_fluc)) +
    geom_line(aes(group = IdLabel, colour = trend), stat = "smooth", method = "loess", alpha = 0.3, size = 0.5) +
    geom_line(data = Down, aes(group = IdLabel, colour = trend), stat = "smooth", method = "loess", alpha = 0.3, size = 0.5) +
    geom_smooth(data = Trends, aes(colour = trend), se = FALSE) +
    ylim(0,1.2) +
    facet_grid(Experiment ~ Period) +
    xlab ("Volume of the fluctuating option") +
    ylab("Proportion of choices to the fluctuating option") +
    geom_vline(data = fixedvol, aes(xintercept = fixedvol), linetype = 2) +
    geom_hline(yintercept = 0.5, linetype = 3) +
    theme_bw()

Trends_model_subj <- Updown %>%
  ungroup() %>% 
  filter(Experiment == "Subjective", 
         #Tracking == "tracker"
         ) %>% 
  select(Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel)

setwd("/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/analysis/R/")

# m.trends.subj <-
#   brm(data = Trends_model_subj, family = bernoulli,
#       sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
#         (1 + Period:trend + Period:sine_vol | Bat), # random slopes 
#       prior = c(prior(normal(0, 5), class = Intercept),
#                 prior(normal(0, 5), class = b),
#                 prior(cauchy(0, 1), class = sd)),
#       iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3, 
#       control = list(adapt_delta = 0.9, max_treedepth = 12),  
#       seed = 12)

# save(m.trends.subj, file = "m.trends.subj.2000iter.rda")
load("m.trends.subj.2000iter.rda")

Trends_model_obj <- Updown %>%
  ungroup() %>% 
  filter(Experiment == "Objective", 
         #Tracking == "tracker"
         ) %>% 
  select(Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel)

# m.trends.obj <-
#   brm(data = Trends_model_obj, family = bernoulli,
#       sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
#         (1 + Period:trend + Period:sine_vol | Bat), # random slopes 
#       prior = c(prior(normal(0, 5), class = Intercept),
#                 prior(normal(0, 5), class = b),
#                 prior(cauchy(0, 1), class = sd)),
#       iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3, 
#       control = list(adapt_delta = 0.9, max_treedepth = 12),  
#       seed = 12)


# save(m.trends.obj, file = "m.trends.obj.3000iter.rda")
load("m.trends.obj.3000iter.rda")

fixef(m.trends.subj)

fixef(m.trends.obj)

##### 
#conditional effects 

int_conditions <- list(Period = c("0.75", "1.5", "3", "6"), trend = c("down", "up"), sine_vol = seq(2, 25, by = 1))

m_trends_subj <- conditional_effects(m.trends.subj, int_conditions = int_conditions, 
                                     spaghetti = T, nsamples = 150, prob = 0.89)

m_trends_obj <- conditional_effects(m.trends.obj, int_conditions = int_conditions, 
                                    spaghetti = T, nsamples = 150, prob = 0.89)

# plotting out the conditional effects 
# the index value for plot can be changed to anything between 1 and 5
# Period vs prop visits to fluctuating option 
plot(m_trends_subj, plot = FALSE, line_args = c(alpha = 1/5)) [[1]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()

# Trend vs prop visits to fluctuating option 
plot(m_trends_obj, plot = FALSE, line_args = c(alpha = 1/5)) [[1]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  xlab("Trend") + 
  theme_bw()

# Volume of fluctuating option vs prop visits to the fluctuating option
plot(m_trends_obj, plot = FALSE, line_args = c(alpha = 1/5)) [[3]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  xlab("Volume of the fluctuating option") + 
  theme_bw()

# Period with trend vs prop visits to fluctuating option 
plot(m_trends_subj, plot = FALSE, line_args = c(alpha = 1/5)) [[4]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()

plot(m_trends_obj, plot = FALSE, line_args = c(alpha = 1/5)) [[4]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()

post <- posterior_samples(m.trends.obj)

mean(post$`b_Period1.5:trendup` < 0)

######

# Attempt 2: setting a minimum noticeable difference. As per the Toelch paper, this 
# is set as a relative intensity of 0.69. Thus we calculate the RI for 
# all the volume pairs. The rationale is as follows. In order to perceive a trend
# at least two observations are needed. We know that this much information can
# be enough to affect the bats' decision-making. Therefore, in order to perceive
# an upward or a downward trend the bats have to have at least two experiences
# of the fluctuating option where the relative intensity was greater than 0.69. 

mindiff_ri <- 0.69
criterion_experience <- mindiff_ri * 3
subj_fixed <- 7.128

Updown_mindiff <- Updown %>% 
  filter(Tracking == "tracker") %>% 
  ungroup() %>%
  group_by(Experiment, IdLabel, Period_day) %>% 
  mutate(rel_intensity = abs((sine_vol - subj_fixed)/((sine_vol + subj_fixed)/2)), 
         rel_intensity = round(rel_intensity, digits = 2)) %>% 
  ungroup() %>% 
  group_by(Experiment, IdLabel, Period_day) %>% 
  mutate(choice_rel_intensity = outFuncLabel * rel_intensity, 
         sum_choice_rel_intensity = choice_rel_intensity + lag(choice_rel_intensity) + lag(lag(choice_rel_intensity))) %>% 
  filter(!is.na(sum_choice_rel_intensity), 
         sum_choice_rel_intensity >= criterion_experience)

Trends_mindiff <- Updown_mindiff %>% 
ungroup() %>% 
  select(Experiment, Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel)

Trends_model_subj_mindiff <- Trends_mindiff %>% 
  filter(Experiment == "Subjective") %>% 
  select(-Experiment)

Trends_model_obj_mindiff <- Trends_mindiff %>% 
  filter(Experiment == "Objective") %>% 
  select(-Experiment)

rm(Trends_mindiff)
  
# Updown_mindiff_experience_lookup <- Updown_mindiff %>% 
#   ungroup() %>% 
#   group_by(Experiment, IdLabel, Period_day) %>% 
#   mutate(sum_rel_int = rel_intensity + lag(rel_intensity) + lag(lag(rel_intensity)), 
#          discriminable = ifelse(sum_rel_int >= 1.38, "yes", "no")) %>% 
#   select(Experiment, IdLabel, Period_day, rows, discriminable)

m.trends.mindiff.subj <-
  brm(data = Trends_model_subj_mindiff, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

save(m.trends.mindiff.subj, file = "m.trends.mindiff.subj.rda")

m.trends.mindiff.obj <-
  brm(data = Trends_model_obj_mindiff, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

int_conditions <- list(Period = c("0.75", "1.5", "3", "6"), trend = c("down", "up"), sine_vol = seq(2, 25, by = 1))

m_trends_mindiff_subj <- conditional_effects(m.trends.mindiff.subj, int_conditions = int_conditions, spaghetti = T, nsamples = 150)

plot(m_trends_mindiff_subj, plot = FALSE, line_args = c(alpha = 1/5)) [[5]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()
#######
# Section 1: Is there a response in the bats' behaviour to the continuously-
# changing environment
#######
# library(season)
# library(broom)

res <- cosinor(stillborn~1, date='dob', data=stillbirth,
              family=binomial(link='cloglog'))

summary_res <- summary(res)
summary_res
summary_res$significant
plot(res)
tibble <- tidy(res$glm)
tibble$p.value

cosinor_stats <- function(sine_choice, cycles) {
  
  result <-cosinor(sine_choice~1, cycles, type = 'hourly', 
                   family=binomial(link='cloglog'))
  
  # tibble(statistic = test$statistic, 
  #        p.value = test$p.value)
  
  tibble <- tidy(result$glm)
  
  tibble(term = tibble$term, 
         estimate = tibble$estimate, 
         p.value = tibble$p.value)
  
}

rhythmicity <- Main_all %>% 
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0), 
         #Date = date(DateTime)
         ) %>% 
  select(DateTime, IdLabel, Period_day, Period, sine_choice) %>% 
  ungroup() %>% 
  group_by(IdLabel, Period_day, Period) %>% 
  # nest(Date = Date,
  #      sine_choice = sine_choice) %>%
  mutate(cycles = 24/Period) %>% 
  mutate(DateTime = as.POSIXct(DateTime),
         sine_choice = as.integer(sine_choice)) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel)

# OK it doesn't work if you map it. Better do it bit by bit for now. 
rhythmicity <- rhythmicity %>% 
  select(-Period_day) %>% 
  ungroup() %>% 
  group_by(Period, IdLabel) %>% 
  nest(DateTime = DateTime,
       sine_choice = sine_choice) %>%
  mutate(res = cosinor(sine_choice ~ 1, date = 'DateTime',
                       data = example, type = 'hourly', cycles = cycles))
  mutate(res = pmap(cycles, cosinor, formula = sine_choice~1, date = 'DateTime', type = 'hourly'))

example <- Main_all %>%
  filter(Period_day == 6) %>%
  ungroup() %>%
  rename(sine_choice = outFuncLabel) %>% 
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
         DateTime = as.POSIXct(DateTime),
         sine_choice = as.integer(ifelse(sine_choice == "sineRewOut", 1, 0))
    ) %>% 
  group_by(IdLabel) %>% 
  mutate(res = group_modify(~cosinor(sine_choice ~1, cycles = 4, type = 'hourly')))

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = example, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)

example <- rhythmicity %>% 
  filter(Period_day == 6) %>% 
  filter(IdLabel == "Bat 45")

res <- cosinor(sine_choice~1, date = 'DateTime',  
               data = example, 
               type = 'hourly', 
               cycles = 4,
               family = binomial(link='cloglog')
)
#####
summary_res$ctable[,4]

# plot(res)

# max_ccf <- function(a,b)
# {
#   d <- ccf(a, b, plot = FALSE)
#   cor = d$acf[,,1]
#   lag = d$lag[,,1]
#   res = data.frame(cor)
#   res_max = res[which.max(res$cor),]
#   return(res_max)
# } 
# 
# max_ccf (b103_3h_d1$sine_vol, b103_3h_d1$smooth)

#####

# THESE ARE THE PLOTS OF INTEREST
# forest plot for the subjective experiment
mcmc_intervals(m.trends.subj, 
               prob_outer = 0.89, 
               pars = vars(1:12), 
               point_size = 1.25) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

# Period with volume vs prop visits to fluctuating option in the subjective exp
plot(m_trends_subj, plot = FALSE, line_args = c(alpha = 0.25)) [[5]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  xlab("Volume of the fluctuating option") + 
  theme_bw()

# forest plot for the objective experiment
mcmc_intervals(m.trends.obj, 
               prob_outer = 0.89, 
               pars = vars(1:12), 
               point_size = 1.25) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

# Period with volume vs prop visits to fluctuating option in the subjective exp

plot(m_trends_obj, plot = FALSE, line_args = c(alpha = 0.25)) [[5]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  xlab("Volume of the fluctuating option") + 
  theme_bw()

######
# Section 2: Is there a difference in the preference for the fluctuating option as part of the upward and downward trend?
# AKA What is the difference between the preference for the 
# fluctuating option as part of the upward and the downward trends as an effect of 
# fixed output, period and volume of the fluctuating option AKA
# The effect of Period, volume of the fluctuating option, and trend on the choice for the 
# fluctuating option
######

# Period with trend vs prop visits to fluctuating option 

plot(m_trends_subj, plot = FALSE, line_args = c(alpha = 1/5)) [[4]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()

plot(m_trends_obj, plot = FALSE, line_args = c(alpha = 1/5)) [[4]] +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  ylab("Proportion of choices to \n the fluctuating option") + 
  theme_bw()

#######
# Section 3: : What is the effect of the fixed option and period on the 
# difference between up and down for preference for the fluctuating 
# option when its equal to the fixed option? The blue dot plot

min_diff <- 0

Trend_2fix_indvis <- Main_all %>% 
  select(DateTime, IdLabel, outFuncLabel, Fixed_steps, sine_steps, sine_vol, reinforce1value, Period, Period_day, Tracking, Experiment) %>%
  group_by(Experiment, Period, Tracking, IdLabel) %>%
  mutate(Prof = case_when(outFuncLabel == "sineRewOut" & reinforce1value >= Fixed_steps ~ 1,
                          outFuncLabel == "sineRewOut" & reinforce1value < Fixed_steps ~ 0,
                          outFuncLabel == "fixRewOut" & reinforce1value >= sine_steps ~ 1,
                          outFuncLabel == "fixRewOut" & reinforce1value < sine_steps ~ 0), 
         sine_vol = round(sine_vol, digits = 0),
         trend = ifelse((sine_vol - lag(sine_vol)) > min_diff, "up", ifelse((sine_vol - lag(sine_vol)) < -(min_diff), "down", "same"))) %>%  
  #calculating whether it is an upwards or downwards trend
  filter(!is.na(trend), #removing NAs
         trend != "same", #filtering out where the volumes were the same
         Tracking == "tracker") %>% #only the trackers
  ungroup() %>%
  mutate( #Adding a column to note which choices are made when the volume pair to choose between is 7 v 13.5      
    Equal = ifelse(Experiment == "Subjective" & sine_vol <= (fixed2 + mindiff) & sine_vol >= (fixed2 - mindiff), 1, 
                   ifelse(Experiment == "Objective" & sine_vol <= (fixed1 + mindiff) & sine_vol >= (fixed1 - mindiff), 1, 0)), 
    #re-labelling Experiment so that it's clear which is fixed and which is fluctuating      
    Experiment = ifelse(Experiment == "Subjective", "Fl = 13.5", "Fx = 13.5")) %>%
  #only selecting the choices we need
  filter(Equal != 1) %>% 
  #this column is redundant now
  select(-Equal) %>% 
  group_by(Experiment, Period, Tracking, IdLabel, trend) %>% 
  mutate(Period = as.factor(Period))

Trend_2fix <- Trend_2fix_indvis %>%
  #Calculating the preference for the higher volume
  summarise(pref_high = mean(Prof)) 

Trend_2fix_diff <- Trend_2fix %>%
  spread(trend, pref_high) %>% 
  mutate(updowndiff = down - up)

Trend_2fix_diff <- Trend_2fix_diff %>% 
  mutate(Experiment = ifelse(Experiment == "Fl = 13.5", "Subjective", "Objective"))

Trend_fixdiff_model_subj <- Trend_2fix_diff %>% 
  ungroup() %>% 
  filter(Experiment == "Subjective") %>% 
  select(Period, IdLabel, updowndiff) %>% 
  rename(Bat = IdLabel) %>% 
  mutate(updowndiff = round(updowndiff, digits = 2), 
         Period = as.factor(Period))
  

Trend_fixdiff_model_obj <- Trend_2fix_diff %>% 
  ungroup() %>% 
  filter(Experiment == "Objective") %>% 
  select(Period, IdLabel, updowndiff) %>% 
  rename(Bat = IdLabel) %>% 
  mutate(updowndiff = round(updowndiff, digits = 2), 
         Period = as.factor(Period))

get_prior(data = Trend_fixdiff_model_obj, 
          family = student, 
          updowndiff ~ Period + (1 | Bat))

# first set the ROPE = what would this be? 

m.fixdiff.subj <-  
  brm(updowndiff ~ Period + (1 | Bat), data = Trend_fixdiff_model_subj, 
      family = student(), 
      prior = c(prior(normal(0, 1), class = "b"), 
                prior(student_t(4, 0, 2.5), class = "sd"), 
                prior(student_t(4, 0, 2.5), class = "sigma")), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 12)

save(m.fixdiff.subj, file = "m.fixdiff.subj.rda")
load("m.fixdiff.subj.rda")
summary(m.fixdiff.subj)

plot(conditional_effects(m.fixdiff.subj))[[1]] + geom_hline(yintercept = 0, linetype = "dashed")

m.fixdiff.obj <-  
  brm(updowndiff ~ Period + (1 | Bat), data = Trend_fixdiff_model_obj, 
      family = student(), 
      prior = c(prior(normal(0, 1), class = "b"), 
                prior(student_t(4, 0, 2.5), class = "sd"), 
                prior(student_t(4, 0, 2.5), class = "sigma")), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 1)

plot(conditional_effects(m.fixdiff.obj))[[1]] + geom_hline(yintercept = 0, linetype = "dashed")
summary(m.fixdiff.obj)
save(m.fixdiff.subj, file = "m.fixdiff.obj.rda")
load("m.fixdiff.obj.rda")

Trend_2fix_indvis_subj <- Trend_2fix_indvis %>% 
  filter(Experiment == "Fl = 13.5")

Trend_2fix_indvis_obj <- Trend_2fix_indvis %>% 
  filter(Experiment == "Fx = 13.5")

m.trends.mindiff.obj <-
  brm(data = Trends_model_obj_mindiff, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

m.fix.indvis.subj <-  
  brm(Prof ~ Period + trend + Period:trend + (1 + Period:trend| IdLabel), 
      data = Trend_2fix_indvis_subj, 
      family = bernoulli, 
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.999, max_treedepth = 15),  
      seed = 12)

save(m.fix.indvis.subj, file = "m.fix.indvis.subj.rda")

m.fix.indvis.obj <-  
  brm(Prof ~ Period + trend + Period:trend + (1 + Period:trend| IdLabel), 
      data = Trend_2fix_indvis_obj, 
      family = bernoulli, 
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.999, max_treedepth = 15),  
      seed = 12)

save(m.fix.indvis.obj, file = "m.fix.indvis.obj.rda")

#######
# Calculating the difference between the trends and how it varies with period 

Trends_diffs_subj <- Trends_model_subj %>%
  ungroup() %>% 
  group_by(Period, Bat, trend, sine_vol) %>% 
  summarise(prop_sine = mean(sine_choice)) %>% 
  pivot_wider(names_from = "trend", values_from = "prop_sine") %>%
  mutate(diff_trends = round((down - up), digits = 2))


get_prior(data = Trends_diffs_subj, 
          diff_trends ~ 1 + Period + (1 + Period|Bat))

m.trends.diff <-
  brm(data = Trends_diffs_subj, family = gaussian,
      # random slopes
      diff_trends ~ Period + sine_vol +  (1 + Period + sine_vol + sine_vol:Period| Bat),
      prior(normal(0, 0.5), class = Intercept), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      seed = 12)

fixef(m.trends.diff)

mcmc_intervals(m.trends.diff, 
               pars = vars(1:4), 
               point_size = 1.75, 
               prob_outer = 0.89)


Trends_diffs_obj <- Trends_model_obj %>%
  ungroup() %>% 
  group_by(Period, Bat, trend, sine_vol) %>% 
  summarise(prop_sine = mean(sine_choice)) %>% 
  pivot_wider(names_from = "trend", values_from = "prop_sine") %>%
  mutate(diff_trends = round((down - up), digits = 2))


get_prior(data = Trends_diffs_obj, 
          diff_trends ~ 1 + Period + (1 + Period|Bat))

m.trends.diff.obj <-
  brm(data = Trends_diffs_obj, family = gaussian,
      # random slopes
      diff_trends ~ Period + sine_vol +  (1 + Period + sine_vol + sine_vol:Period| Bat),
      prior(normal(0, 0.5), class = Intercept), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      seed = 12)

fixef(m.trends.diff.obj)

mcmc_intervals(m.trends.diff.obj, 
               pars = vars(1:4), 
               point_size = 1.75, 
               prob_outer = 0.89)


  

#####
# A measure of tracking - NRMSE
#####
median_intervisit_time <- Main_all %>% 
  ungroup() %>% 
  group_by(Experiment, Period, IdLabel) %>% 
  mutate(ivi = (timediff - lag(timediff))*3600) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day) %>% 
  summarise(med_ivi = median(ivi, na.rm = TRUE))

######
# binning in 1 minute bins 
######
# Objective mean
binratio <- 1/18 #set this for a binsize that is a proportion of the period
binsize_min <- 1 #set this for a fixed binsize in units minutes
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
  unique() %>%
  arrange(IdLabel)

#this is the code for setting the binsize as fixed across all periods. The code for doing it proportionally is not yet written
timeseries1 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-19 15:00:00"), to = as.POSIXct("2020-06-27 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries2 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-29 15:00:00"), to = as.POSIXct("2020-07-10 03:00:00"), by = as.numeric(binsize[1,5])))

timeseries <- bind_rows(timeseries1, timeseries2) %>%  
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  filter(Hour >= 15 | Hour < 3) %>% #Filtering and removing the non-experimental times
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
seq <- c(2:9, 11:21)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins)

rm(timeseries1, timeseries2)

Main_all <- Main_all %>%
  group_by(Period_day, IdLabel) %>%
  mutate(time_bins = floor_date(as_datetime(DateTime), unit = "1 minute"))

#Creating a separate table for the workrate binning
onemin_bins_obj_intm <- Main_all %>%
  filter(Experiment == "Objective") %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, time_bins, Tracking)
#Finally, adding the time series data to the experimental data
onemin_bins_obj <- full_join(timeseries, onemin_bins_obj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(Hour = hour(time_bins)) %>%
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
  arrange(IdLabel, Day, time_bins)

#Creating a separate table for the workrate binning
onemin_bins_subj_intm <- Main_all %>%
  filter(Experiment == "Subjective") %>% 
  select(DateTime, Period, Period_day, IdLabel, vis_vol, time_bins, Tracking)
#Finally, adding the time series data to the experimental data
onemin_bins_subj <- full_join(timeseries, onemin_bins_subj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(Hour = hour(time_bins)) %>%
  filter(Hour < 2 | Hour >= 14) %>%
  select(-Hour) %>%
  mutate(Experiment = "Subjective")

rm(onemin_bins_subj_intm, onemin_bins_obj_intm)
onemin_bins <- bind_rows(onemin_bins_subj, onemin_bins_obj) %>% 
  select(-outFuncLabel)

onemin_bins_summ <- onemin_bins %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, time_bins) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day) %>% 
  summarise(median = median(count), 
            mean = mean(count))

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
  unique() %>%
  arrange(IdLabel)

#this is the code for setting the binsize as fixed across all periods. The code for doing it proportionally is not yet written
timeseries1 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-19 15:00:00"), to = as.POSIXct("2020-06-27 03:00:00"), by = as.numeric(binsize[1,5])))
timeseries2 <- 
  data.frame(Datetime = seq.POSIXt(from = as.POSIXct("2020-06-29 15:00:00"), to = as.POSIXct("2020-07-10 03:00:00"), by = as.numeric(binsize[1,5])))

timeseries <- bind_rows(timeseries1, timeseries2) %>%  
  mutate(Datetime = as.POSIXct(Datetime), 
         Hour = hour(Datetime)) %>%
  filter(Hour >= 15 | Hour < 3) %>% #Filtering and removing the non-experimental times
  select(-Hour) %>%
  rename(time_bins = Datetime)
timeseries <- bind_rows(replicate(8, timeseries, simplify = FALSE))

l <- nrow(timeseries)
seq <- c(2:9, 11:21)

timeseries <- timeseries %>%  
  mutate(Period_day = rep(c(0.75, "0.75 Rev", 1.5, "1.5 Rev", 3, "3 Rev", 6, "6 Rev"), each = l/8)) %>%
  mutate(Day = rep(seq, each = binsize[1,6], times = 8))

timeseries <- left_join(Datetimes, timeseries, by = c("Day", "Period_day")) %>%
  filter(!is.na(IdLabel)) %>%
  arrange(IdLabel, Day, time_bins) %>% 
  filter(!is.na(time_bins))

rm(timeseries1, timeseries2)

Main_all <- Main_all %>%
  group_by(Period_day, IdLabel) %>%
  mutate(time_bins = floor_date(as_datetime(DateTime), unit = "5 minute"))

#Creating a separate table for the workrate binning
fivemin_bins_obj_intm <- Main_all %>%
  filter(Experiment == "Objective", 
         Tracking == "tracker") %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking)

#Finally, adding the time series data to the experimental data
fivemin_bins_obj <- left_join(timeseries, fivemin_bins_obj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(Hour = hour(time_bins)) %>%
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
  arrange(IdLabel, Day, time_bins)

#Creating a separate table for the workrate binning
fivemin_bins_subj_intm <- Main_all %>%
  filter(Experiment == "Subjective", 
         Tracking == "non-tracker") %>% 
  select(DateTime, Period, Period_day, IdLabel, outFuncLabel, sine_vol, time_bins, Tracking)

# Finally, adding the time series data to the experimental data
fivemin_bins_subj <- left_join(timeseries, fivemin_bins_subj_intm, by = c("Period_day", "Tracking", "IdLabel", "time_bins")) %>%
  mutate(Hour = hour(time_bins)) %>%
  filter(Hour < 2 | Hour >= 14) %>%
  select(-Hour) %>%
  mutate(Experiment = "Subjective")

rm(fivemin_bins_subj_intm, fivemin_bins_obj_intm)
fivemin_bins <- bind_rows(fivemin_bins_subj, fivemin_bins_obj)

fivemin_bins_summ <- fivemin_bins %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, time_bins) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day) %>% 
  summarise(median = median(count), 
            mean = mean(count))
#####

nrmse_func <-  function(obs, pred, type = "sd") {

  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

rae_func <-  function(obs, pred) {
  
rae_numrt <- sum(abs(obs - pred))
rae_denom <- sum(abs(mean(obs) - obs))
rae <- rae_numrt/rae_denom
rae <- round(rae, digits = 2)
return(rae)
  
}

rrse_func <-  function(obs, pred) {
  
  rrse_numrt <- sum((obs - pred)^2)
  rrse_denom <- sum((mean(obs) - obs)^2)
  rrse <- sqrt(rrse_numrt/rrse_denom)
  return(rrse)
  
}

######

min_sinevol <- 1.944
max_sinevol <- 24.624

fivemin_bins <- fivemin_bins %>% 
  filter(!is.na(sine_vol)) %>% 
  mutate(sine_choice = ifelse(outFuncLabel == "sineRewOut", 1, 0), 
         sine_norm = (sine_vol - min_sinevol)/(max_sinevol - min_sinevol), 
         sine_norm = round(sine_norm, digits = 2)) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel, time_bins) %>% 
  summarise(sine_choice = mean(sine_choice), 
            sine_norm = mean(sine_norm))


Amplitude <- 35
Disp <- 41

fivemin_bins_averages <- fivemin_bins %>% 
  ungroup() %>%
  group_by(Period_day, Period, IdLabel, time_bins) %>% 
  summarise(mean_sine_choice = mean(sine_choice)) %>% 
  mutate(timediff = 300, 
         timediff = cumsum(timediff), 
         td = timediff - 150, 
         Period = Period * 3600) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(sine_vol = 0.5 * sin(2 * pi * (1 / Period) * td + (pi / 2)) + 0.5)

fivemin_bins_averages %>% 
  filter(IdLabel == "Bat 101", 
         Period_day == "0.75") %>% 
  ggplot() +
  geom_line(aes(td, sine_vol)) + 
  geom_line(aes(td, mean_sine_choice), colour = "red")

  # mutate(sine_vol = 0.5 * sin(2 * pi * (1 / Period) * td + (pi / 2)) + 0.5)

calc <- fivemin_bins %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>%
  summarise(nrmse = nrmse_func(sine_choice, sine_norm, type = "sd"), 
            rae = rae_func(sine_choice, sine_norm), 
            rrse = rrse_func(sine_choice, sine_norm)) %>% 
  filter(nrmse != "Inf") %>% 
  pivot_longer(cols = c(4:6), names_to = "calculations", values_to = "values")

calc %>% 
  filter(calculations != "rae") %>% 
  ggplot() + 
  geom_jitter(aes(IdLabel, values, color = calculations, group = calculations), alpha = 0.5, size = 0.5) +
  ylim(0, 10) + 
  facet_wrap(.~Period_day) + 
  theme_bw()

calc_lagone <- fivemin_bins %>% 
  mutate(lag_one = lag(sine_choice)) %>% 
  filter(!is.na(lag_one)) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>%
  summarise(nrmse = nrmse_func(lag_one, sine_norm, type = "sd")) %>% 
  filter(nrmse != "Inf")

calc_lagmore <- fivemin_bins %>% 
  mutate(lag_one = lag(sine_choice, 5)) %>% 
  filter(!is.na(lag_one)) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>%
  summarise(nrmse = nrmse_func(lag_one, sine_norm, type = "sd")) %>% 
  filter(nrmse != "Inf")

calc_lead <- fivemin_bins %>% 
  mutate(lag_one = lead(sine_choice, 3)) %>% 
  filter(!is.na(lag_one)) %>% 
  ungroup() %>% 
  group_by(Experiment, Period_day, IdLabel) %>%
  summarise(nrmse = nrmse_func(lag_one, sine_norm, type = "sd")) %>% 
  filter(nrmse != "Inf")

calc %>% 
  filter(calculations == "nrmse") %>% 
  ggplot() + 
  geom_jitter(aes(IdLabel, values), colour = "red", alpha = 0.5, size = 0.5) +
  geom_jitter(data = calc_lead, aes(IdLabel, nrmse), colour = "blue", 
              alpha = 0.5, size = 0.5) +
  ylim(0, 10) + 
  facet_wrap(.~Period_day) + 
  theme_bw()

# there has to be a way to do this iteratively and figure it out properly which is the lowest 
#####
# a fresh attempt at the trend models 
######

Trends_model_obj <- Trends_model_obj %>% 
  mutate(Experiment = "Objective")

Trends_model_subj <- Trends_model_subj %>% 
  mutate(Experiment = "Subjective")

Trends_model <- bind_rows(Trends_model_obj, Trends_model_subj) %>% 
  filter(trend != "same") 

# taking the real tracking bats from the table in the other script

bats_to_keep <- sidepref_rewmax_nrmse_keep %>% 
  ungroup() %>% 
  select(Experiment, IdLabel) %>% 
  rename(Bat = IdLabel) %>% 
  distinct() %>% 
  mutate(keep = "yes")

Trends_model <- Trends_model %>% 
  left_join(bats_to_keep, by = c("Experiment", "Bat"))

Trends_model  <- Trends_model %>% 
  filter(keep == "yes") %>% 
  select(-keep) %>% 
  rename(Contrast = Experiment) %>%
  mutate(Contrast = ifelse(Contrast == "Objective", "Low", "High"))

# m.trends.all <-
#   brm(data = Trends_model, family = bernoulli,
#       #sine_choice ~ sine_vol + (1 + trend:sine_vol|Period) + (1 + trend:sine_vol|Contrast), # random slopes
#       sine_choice ~ sine_vol + Period + Contrast + trend:sine_vol + 
#         Period:trend + (1 + Period:trend|Bat), 
#       # should allow intercept to vary by bat 
#       prior = c(prior(normal(0, 5), class = Intercept),
#                 prior(normal(0, 5), class = b),
#                 prior(cauchy(0, 1), class = sd)
#                 ),
#       iter = 3000, warmup = 1500, chains = 4, cores = 4, thin = 3,
#       control = list(adapt_delta = 0.9, max_treedepth = 12),
#       seed = 12)
check <- conditional_effects(m.trends.all)

save(m.trends.all, file = "m.trends.interceptbybat.2000iter.rda")

save(m.trends.all, file = "m.trends.periodtrendbybat.3000iter.rda")

load("m.trends.periodtrendbybat.3000iter.rda")

m.varyingslopes <- load("m.trends.periodtrendbybat.2000iter.rda")
m.varyingintercepts <- m.trends.all
m.varying.slopes <- m.trends.all

m.varying.slopes <- add_criterion(m.varying.slopes, "waic")
m.varyingintercepts <- add_criterion(m.varyingintercepts, "waic")


w <- loo_compare(m.varying.slopes, m.varyingintercepts, criterion = "waic") %>% 
  print(simplify = F)

plot(conditional_effects(m.trends.all))

#### Vladi approved models below: 
# Trends models

m.trends.allvaryingslopes <-
  brm(data = Trends_model, family = bernoulli,
      sine_choice ~ sine_vol + Period + Contrast + trend:sine_vol +
        Period:trend + (1 + Period:trend + trend:sine_vol|Bat),
      # should allow intercept to vary by bat
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)
                ),
      iter = 3000, warmup = 1500, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

save(m.trends.allvaryingslopes, file = "m.trends.allvaryingslopes.rda")

m.trends.somevaryingslopes <-
  brm(data = Trends_model, family = bernoulli,
      sine_choice ~ sine_vol + Period + Contrast + trend:sine_vol +
        Period:trend + (1 + Period:trend|Bat),
      # should allow intercept to vary by bat
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)
      ),
      iter = 3000, warmup = 1500, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

save(m.trends.somevaryingslopes, file = "m.trends.somevaryingslopes.rda")

load("m.trends.somevaryingslopes.rda")
load("m.trends.allvaryingslopes.rda")

m.trends.somevaryingslopes <- add_criterion(m.trends.somevaryingslopes, "waic")
m.trends.allvaryingslopes <- add_criterion(m.trends.allvaryingslopes, "waic")

save(m.trends.somevaryingslopes, file = "m.trends.somevaryingslopes.rda")
save(m.trends.allvaryingslopes, file = "m.trends.allvaryingslopes.rda")

w <- loo_compare(m.trends.somevaryingslopes, m.trends.allvaryingslopes, criterion = "waic") %>% 
  print(simplify = F)

post_b <- posterior_samples(m.trends.allvaryingslopes, pars = "^b", add_chain = T)
post_sd <- posterior_samples(m.trends.allvaryingslopes, pars = "^sd", add_chain = T)
post_r <- posterior_samples(m.trends.allvaryingslopes, pars = "^r", add_chain = T)

color_scheme_set("orange")

post_b %>%
  #select(contains("trenddown") & contains("Period1.5")) %>% 
  mcmc_trace(facet_args = list(ncol = 4))

neff_ratio(m.trends.allvaryingslopes) %>% 
  mcmc_neff() 

mcmc_intervals(m.trends.allvaryingslopes,
               pars = vars(1:10),
               prob_outer = 0.89, 
               point_size = 1.1
)

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

######
# NRMSE models
nrmse_model <- sidepref_rewmax_nrmse_keep %>% 
  filter(strategy == "maximizing_strategy") %>% 
  ungroup() %>% 
  select(Experiment, Period, IdLabel, nrmse) %>% 
  group_by(Experiment, IdLabel, Period) %>% 
  #summarise(nrmse = sum(nrmse)) %>% 
  mutate(Experiment = ifelse(Experiment == "Objective", "Low", "High")) %>%
  rename(Bat = IdLabel, 
         Contrast = Experiment) %>% 
  mutate (Period = as.factor(Period)) %>% 
  # filtering the outliers
  filter(nrmse < 20)

get_prior(data = nrmse_model, 
          formula = nrmse ~ Period + Contrast + Period:Contrast + (1 + Period|Bat), 
          family = Gamma(link = "inverse"))
  
# what's the distribution of nrmse values OK 

nrmse_model %>%  
  filter(nrmse < 5) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_density(aes(nrmse, group = Period, colour = Period)) + 
  facet_grid(.~Contrast) + 
  xlab("NRMSE values") + 
  ylab("Density")

m.nrmse <-
  brm(data = nrmse_model, family = lognormal, 
      nrmse ~ Period + Contrast + (1 + Period|Bat),
      prior = c(prior(normal(0,5), class = Intercept),
                prior(normal(0,5), class = b),
                prior(cauchy(0,1), class = sd)
      ), 
      # prior = c(prior(student(3, 0, 2), class = Intercept),
      #           prior(student(3, 0, 2), class = b),
      #           prior(cauchy(0, 1), class = sd),
      #           prior(cauchy(0, 1), class = sigma), 
      #           #prior(lkj(2), class = cor),
      # ),
      #save_pars = save_pars(all = TRUE),
      iter = 8000, warmup = 4000, chains = 1, cores = 4, thin = 3,
      control = list(adapt_delta = 0.999, max_treedepth = 15),
      seed = 12)
plot(conditional_effects(m.nrmse))
save(m.nrmse, file = "m.nrmse.rda")

m.nrmse.interaction <-
  brm(data = nrmse_model, family = lognormal, 
      nrmse ~ Period + Contrast + Period:Contrast + (1 + Period|Bat),
      prior = c(prior(normal(0, 0.5), class = Intercept),
                prior(normal(0, 0.5), class = b), 
                prior(cauchy(0, 0.5), class = sd), 
                prior(lkj(2), class =cor)
      ), 
      iter = 8000, warmup = 4000, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 15),
      seed = 12)

post <- posterior_samples(m.nrmse.interaction, pars = "^r", add_chain = T)

color_scheme_set("orange")

post %>%
  select(contains("Intercept")) %>% 
  mcmc_trace(facet_args = list(ncol = 4))

save(m.nrmse.interaction, file = "m.nrmse.interaction.rda")

m.nrmse <-
  brm(data = nrmse_model, family = gaussian,
      nrmse ~ Period + Contrast + (1 + Period|Bat) + 
        (1 + Contrast|Bat),
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b), 
                prior(cauchy(0, 1), class = sd), 
                prior(lkj(2), class =cor)
      ), 
      # NEED TO DO A PRIOR PREDICTIVE SIMULATION FOR THIS?
      # should allow intercept to vary by bat
      # prior = c(prior(normal(0, 5), class = Intercept),
      #           prior(normal(0, 5), class = b),
      #           prior(cauchy(0, 1), class = sd)
      # ),
      iter = 3000, warmup = 1500, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9, max_treedepth = 12),
      seed = 12)

load("m.nrmse.interaction.rda")
load("m.nrmse.rda")

mcmc_intervals(m.nrmse.interaction, 
               pars = vars(1:8),
               prob_outer = 0.89, 
               point_size = 1.1
)

plot(conditional_effects(m.nrmse.interaction), prob = 0.89)

