#####
# Statistical analyses
#####
rm(list = ls())

require(tidyverse)
require(brms)
#require(season)

#######
# Reading in and preparing data 
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
  # first removing the original outFuncLabel column
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
# Question 1: What do the bats match to? 
########
# Step 1: creating virtual bats with different strategies

# setting the pump conversion step 
step_conv <- 0.324

# creating a data table with the sine wave, and a period of 3 hours
wave <- tibble(rown = c(1:360), 
               timediff = 0,
               Amplitude = 35, 
               Disp = 41, 
               Period = 3,
               # sticking with the fixed option = 7 microLitres as this was what 
               # was done with the pilot bats
               fixed = round(22 * step_conv, digits = 2)) %>%
  # removing the old row counter so it can be done over
  select(-rown) %>%
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
    sine_vol = round((Amplitude * sin(2 * pi * (1 / Period) * wavetime + (pi / 2)) + Disp) * step_conv, digits = 2),
    Period = Period / 3600
  ) %>%
  # removing time points that occurred after 12 hours by the calculation
  filter(timediff <= 12) %>%
  # removing the now-unnecessary columns
  select(-wavetime, -rown) %>% 
  mutate(visits = 1:n(), 
         # calculating the relative intensity 
         rel_int = round((sine_vol - fixed)/((sine_vol + fixed)/2), digits = 2)
  )

#######
# creating strategy one: simple reinforcement learning model. Bats create an estimate of the value of 
# each option, and sample at a certain rate. The choice is done based on the 
# absolute difference of the two options
########

# 1. Function for calculating the softmax value - 
#this produces the P(i) or the probability of visiting a flower i
p_chooser <- function(probs) { 
  # this function is to help the bat choose between the two probabilities
  p <- runif(1, 0, 1)
  upper_probs <- cumsum(probs)
  lower_probs <- lag(upper_probs, default = 0)
  which(map2_lgl(.x = lower_probs, .y = upper_probs, ~between(p, .x, .y)))
}

draw_reward_from_prob <- function(prob) {
  rbinom(1, 1, prob)
}

softmax <- function(estimates, temperature) {
  exps <- exp(estimates / temperature)
  exps / sum(exps)
}

choose_softmax <- function(estimates, temperature) {
  probs <- softmax(estimates, temperature = temperature)
  p_chooser(probs)
}

######
# 2. Function to check if the sum of visits at the mask == 1 locations have reached the target
over_target_visits <- function(vis_vec, mask, target) {
  sum(sum(vis_vec[mask == 1]) >= target)
}
######
#3. function to simulate the reinforcement learning model 
update_memo <- function(param, old) {
  estimates <- c(param$estimates1, param$estimates2)
  loc_visited <- choose_softmax(estimates, param$temperature) # where to go
  # tally visits
  param$n_visited <- param$n_visited + 1
  # remember where last visit happened
  #what is the reward?  
  reward1 <- ifelse(loc_visited == 1, param$option1, 0)
  reward2 <- ifelse(loc_visited == 2, param$option2, 0)
  #what are the estimates 
  param$estimates1 <- 
    case_when(loc_visited == 1 ~ (((1 - param$l_rate) * param$estimates1) + param$l_rate * reward1), 
              loc_visited == 2 ~ param$estimates1)
  param$estimates2 <- 
    case_when(loc_visited == 1 ~ param$estimates2, 
              loc_visited == 2 ~ (((1 - param$l_rate) * param$estimates2) + param$l_rate * reward2)) 
  #putting them all together into one estimate list 
  #output is the estimate list 
  #param$reward <- reward
  param$loc <- loc_visited
  param$option1 <- old$option1
  param$option2 <- old$option2
  param
}

n_ind <- 20 # number of individuals to simulate over 

estimator_sim_sine <- cross_df(list(l_rate = c(0.1, 0.5, 0.9),
                                    temperature = c(2, 5, 10, 15), 
                                    ind = c(seq(1:n_ind)))) %>% 
  mutate(option1 = list(wave$fixed), 
         option2 = list(wave$sine_vol), 
         cond = paste(l_rate, temperature, sep = ";")) %>% 
  unnest(cols = c(option1, option2)) %>% 
  mutate(loc = 0, 
         estimates1 = 0, 
         estimates2 = 0, 
         n_visited = 0) %>% 
  mutate(memories = pmap(list(l_rate = l_rate, temperature = temperature, option1 = option1, 
                              option2 = option2, loc = loc, 
                              estimates1 = estimates1, 
                              estimates2 = estimates2, 
                              n_visited = n_visited), list)) %>% 
  select(ind, cond, memories) %>% 
  group_by(cond, ind) %>% 
  mutate(memories = accumulate(memories, .f = update_memo)) 
######
estimator_sine <- estimator_sim_sine %>% 
  mutate(loc = map(memories, ~pluck(., "loc")), 
         option1 = map(memories, ~pluck(., "option1")), 
         option2 = map(memories, ~pluck(., "option2")), 
         #n_visited = map(memories, ~pluck(., "n_visited")), 
         estimates1 = as.numeric(map(memories, ~pluck(.x, "estimates1"))), 
         estimates2 = as.numeric(map(memories, ~pluck(.x, "estimates2"))), 
         estimates1 = round(estimates1, digits = 3), 
         estimates2 = round(estimates2, digits = 3)) %>% 
  select(-memories)

estimator_sine <- estimator_sine %>% 
  filter(loc != 0) %>% 
  mutate(chosen = ifelse(loc == 1, 1.944, 24.624), 
         chosen = as.numeric(chosen), 
         cond2 = cond) %>%
  separate(cond2, into = c("l_rate", "temperature"), sep = ";") %>%
  group_by(ind, cond) %>% 
  mutate(visits = 1:n()) %>% 
  left_join(wave, by = c("visits")) %>% 
  ungroup() %>% 
  select(!c(Amplitude, Disp, Period, fixed, cond)) %>% 
  group_by(ind, l_rate, temperature) %>% 
  mutate(sum_choices = sum(chosen), 
         Tracking = ifelse(sum_choices > 700 & sum_choices < 8871, "tracker", "non-tracker"))

estimator_sine %>% 
  filter(Tracking == "tracker") %>%
  ungroup() %>% 
  group_by(ind, l_rate, temperature) %>%
  mutate(smooth = as.numeric(tsSmooth(StructTS(chosen, type = "level")))) %>%
  ggplot(aes(timediff)) +
  #geom_point(aes(y = chosen, colour = as.factor(ind), group = as.factor(ind)), alpha = 0.25, size = 0.2) +
  #geom_point(aes(y = chosen), colour = "red", alpha = 0.25, size = 0.1) + 
  #geom_line(aes(y = smooth), color = "cornflowerblue") +
  geom_line(aes(y = smooth, color = ind, group = ind), alpha = 0.5) + 
  geom_line(aes(y = sine_vol)) +
  #scale_x_continuous(breaks = seq(0,12,1)) +
  geom_hline(yintercept = 7, linetype = 2) +
  xlab("Hour") +
  ylab(expression(paste("Volume output of the variable option [", mu, "L]"))) +
  # scale_y_continuous(limits = c(minsine, maxsine), 
  #                    sec.axis = sec_axis(~.,
  #                                        breaks = c(minsine, maxsine),
  #                                        labels = optionchoice)) + 
  facet_grid(temperature ~ l_rate, scales = "free_y") +
  theme_classic() +
  theme(plot.title = element_text(size=12)) +
  theme(strip.placement = "outside")

########
# creating strategy two: simple reinforcement learning model. Bats create an estimate of the value of 
# each option, and sample at a certain rate. The choice is done based on the 
# relative intensity of the two options
########
########
# creating strategy three: a bat that knows the value of the fixed option. 
# This bat starts at random, then switches to the fluctuating option until it is 
# indistinguishable from the fixed option. Then the bat samples at random until 
# there is a noticeable difference. Then it switches to the fixed option but 
# samples periodically. When there is once again a noticeable difference the bat switches to the fluctuating option 
#######

# setting the just-noticeable difference in relative intensity

jndiff <- 0.5

fixed <- function() {
  
}
known_fixed <- function(rel_int, visits, fixed, sine_vol) {
  loc <- case_when(visits == 1 ~ sample(c(1,2), 1, replace = F), 
                   visits > 1 & rel_int > jndiff ~ 2, 
                   rel_int < -jndiff ~ sample(c(1,2), 1, replace = F, prob = 0.75), 
                   rel_int < jndiff & rel_int > -jndiff ~ sample(c(1,2), 1, replace = F)
  )
}

choose_softmax <- function(estimates, temperature) {
  probs <- softmax(estimates, temperature = temperature)
  p_chooser(probs)
}

update_memo <- function(param, old) {
  estimates <- c(param$estimates1, param$estimates2)
  loc_visited <- choose_softmax(estimates, param$temperature) # where to go
  # tally visits
  param$n_visited <- param$n_visited + 1
  # remember where last visit happened
  #what is the reward?  
  reward1 <- ifelse(loc_visited == 1, param$option1, 0)
  reward2 <- ifelse(loc_visited == 2, param$option2, 0)
  #what are the estimates 
  param$estimates1 <- 
    case_when(loc_visited == 1 ~ (((1 - param$l_rate) * param$estimates1) + param$l_rate * reward1), 
              loc_visited == 2 ~ param$estimates1)
  param$estimates2 <- 
    case_when(loc_visited == 1 ~ param$estimates2, 
              loc_visited == 2 ~ (((1 - param$l_rate) * param$estimates2) + param$l_rate * reward2)) 
  #putting them all together into one estimate list 
  #output is the estimate list 
  #param$reward <- reward
  param$loc <- loc_visited
  param$option1 <- old$option1
  param$option2 <- old$option2
  param
}

known_fixed_sine <- wave %>% 
  mutate(loc = known_fixed(rel_int, visits, fixed, sine_vol))
# reading data from the real bats - pilot data 

# comparing the fake and the real bats - which is the best fit? calculating a score for matching - cross-correlation

#######
# Question 2: The effect of period and fixed output on matching 
#######
# analysis 1: 
# comparing the smoothed wave of the bats' choices to the sinewave
# taking the data from the subjective mean experiment

# cross_corr_subj <- Main_all %>% 
#   filter(Experiment == "Subjective", 
#          Tracking == "tracker") %>% 
#   # taking the relevant columns - the sinewave is here is the one experienced by the
#   # bats, not the calculated one. 
#   select(Period_day, IdLabel, chosen, sine_vol) %>% 
#   filter(!is.na(sine_vol)) %>% 
#   # calculating the smoothed 'wave' of the bats' choices 
#   mutate(smooth = as.numeric(tsSmooth(StructTS(chosen, type = "level"))))

# analysis 2: 
# figuring out the effect of the period and cycle number on the choice of the bats 

period_model_subj <- Main_all %>% 
  filter(Experiment == "Subjective", 
         Tracking == "tracker") %>% 
  ungroup() %>%
  # taking the relevant columns - the sinewave is here is the one experienced by the
  # bats, not the calculated one. 
  select(DateTime, timediff, Period_day, Period, sine_vol, IdLabel, chosen
  ) %>% 
  arrange(Period_day, IdLabel) %>% 
  filter(!is.na(sine_vol)) %>% 
  mutate(sine_vol = round(sine_vol, digits = 1), 
         timediff = round(timediff, digits = 2), 
         chosen = ifelse(chosen == 1.944, 0, 1), 
         Day = ifelse(str_detect(Period_day, "Rev"), 2, 1), 
         #wave_number = (timediff%/%Period) + 1
  ) %>% 
  select(Period, Day, DateTime,
         sine_vol, IdLabel, chosen) %>% 
  mutate(Period = as.factor(Period)) %>% 
  rename(Bat = IdLabel, 
         sine_choice = chosen)
# ungroup() %>% 
# group_by

m.period.subj <-
  # is period categorical or continuous?
  brm(data = period_model_subj, family = bernoulli,
      formula = sine_choice ~ Period + Day + sine_vol + Period:sine_vol + 
        (1 + Period + Day + sine_vol + Period:sine_vol| Bat), # random slopes
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4, thin = 3,
      control = list(adapt_delta = 0.9995, max_treedepth = 15),
      seed = 12)

period_model_obj <- Main_all %>% 
  filter(Experiment == "Objective", 
         Tracking == "tracker") %>% 
  ungroup() %>% 
  # taking the relevant columns - the sinewave is here is the one experienced by the
  # bats, not the calculated one. 
  select(timediff, Period_day, Period, sine_vol, IdLabel, chosen) %>% 
  arrange(Period_day, IdLabel) %>% 
  filter(!is.na(sine_vol)) %>% 
  mutate(sine_vol = round(sine_vol, digits = 1), 
         timediff = round(timediff, digits = 2), 
         chosen = ifelse(chosen == 1.944, 0, 1), 
         Day = ifelse(str_detect(Period_day, "Rev"), 2, 1), 
         wave_number = (timediff%/%Period) + 1) %>% 
  select(Period, Day, wave_number, sine_vol, IdLabel, chosen) %>% 
  mutate(Period = as.factor(Period)) %>% 
  rename(Bat = IdLabel, 
         sine_choice = chosen)

m.period.obj <- 
  brm(data = period_model_obj, family = binomial,
      sine_choice | trials(1) ~ Period + Day + wave_number + 
        sine_vol + (1 + Period + trend + sine_vol | Bat), # random slopes 
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 12)

example <- period_model_subj %>%
  filter(Bat == "Bat 22",
         Day == 2,
         Period == 1.5) %>%
  ungroup() %>%
  mutate(
    #DateTime = format(as.POSIXct(DateTime), format = "%H:%M:%S"),
         DateTime = as.POSIXct(DateTime),
         sine_choice = as.integer(sine_choice),
         chosen = ifelse(sine_choice == 0, 2, 25),
         smooth = as.integer(tsSmooth(StructTS(chosen, type = "level")))
    )

example <- as.data.frame(example)

res <- cosinor(chosen~1, 
               date='DateTime', type = 'hourly',
              data=example, cycles = 8, family = poisson()
              )
summary(res)
plot(res)
# 
res = cosinor(stillborn~1, date='dob', data=stillbirth,
              family=binomial(link='cloglog'))
# 
# summary(res)
# plot(res)
# 
# 
# 
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
########
# Question 3: What was the effect of period and fixed output on the choice 
# for the profitable option? - probably not necessary
########
# is this necessary analysis? 
########
# Question 4: Fitting the sine wave
#######
test <- Main_all %>% 
  filter(IdLabel == "Bat 74", 
         Period_day == 6) %>% 
  select(IdLabel, sine_vol, chosen, Period, Disp, timediff) %>% 
  mutate(chosen = ifelse(chosen == 1.944, 7, sine_vol),
         #smooth = as.numeric(tsSmooth(StructTS(chosen, type = "level"))), 
         Period = 3*3600, 
         timediff = round(timediff*3600), 
         Disp = Disp * 0.324) %>% 
  filter(!is.na(sine_vol))

# sine_fit <- lm(smooth ~ sin((2*pi/Period)*timediff)+cos((2*pi/Period)*timediff),data=test)
# sine_fit <- lm(smooth ~ ,data=test)

nls.mod <-nls(chosen ~ a + b*sin((2*pi/Period*timediff) + Disp), start=list(a = 1, b = 1), data = test, trace = TRUE)
summary(nls.mod)

######
# Question 4: What is the difference between the preference for the 
# fluctuating option as part of the upward and the downward trends as an effect of 
# fixed output, period and volume of the fluctuating option AKA
# The effect of Period, volume of the fluctuating option, and trend on the choice for the 
# fluctuating option
######

mindiff <- 0 

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
  mutate(#sine_vol = round(sine_vol),
    Prof = case_when(outFuncLabel == "sineRewOut" & reinforce1value >= Fixed_steps ~ 1,
                     outFuncLabel == "sineRewOut" & reinforce1value < Fixed_steps ~ 0,
                     outFuncLabel == "fixRewOut" & reinforce1value >= sine_steps ~ 1,
                     outFuncLabel == "fixRewOut" & reinforce1value < sine_steps ~ 0), 
    # one of the following lines can be commented out depending on how 'up' and 'down' are classified
    trend = ifelse((sine_vol - lag(sine_vol)) > mindiff, "up", ifelse((sine_vol - lag(sine_vol)) < -(mindiff), "down", "same")), 
    # accounting for the difference
    outFuncLabel = ifelse(outFuncLabel == "sineRewOut", 1, 0)) %>% 
  filter(!is.na(trend))
  # group_by(Experiment, Period, Tracking, IdLabel, trend, sine_vol) %>%
  # summarise(pref_fluc = mean(outFuncLabel))


Trends_model_subj <- Updown %>%
  ungroup() %>% 
  filter(Experiment == "Subjective", 
         Tracking == "tracker") %>% 
  select(Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel)

setwd("/Users/shambhavi/Google Drive/Experiments & Data/Environment_tracking_2019_2020/analysis/R")

m.trends.subj <-
  brm(data = Trends_model_subj, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes 
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 1500, warmup = 700, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.99, max_treedepth = 20),  
      seed = 12)

save(m.trends.subj, file = "model2.rda")

load("model2.rda")
plot(m.trends.subj)
fixef(m.trends.subj)
summary(m.trends.subj)
neff_ratio(m.trends.subj) %>% 
  mcmc_neff() + 
  theme_bw()

m.trends.subj <-
  brm(data = Trends_model_subj, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes 
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9, max_treedepth = 12),  
      seed = 12)

save(m.trends.subj, file = "m.trends.subj.3000iter.rda")

Trends_model_obj <- Updown %>%
  ungroup() %>% 
  filter(Experiment == "Objective", 
         Tracking == "tracker") %>% 
  select(Period, IdLabel, trend, sine_vol, outFuncLabel) %>% 
  mutate(sine_vol = round(sine_vol), 
         Period = as.factor(Period)) %>% 
  arrange(Period, IdLabel) %>% 
  rename(Bat = IdLabel, 
         sine_choice = outFuncLabel)

m.trends.obj <-
  brm(data = Trends_model_obj, family = bernoulli,
      sine_choice ~ Period + trend + sine_vol + Period:trend + Period:sine_vol +
        (1 + Period:trend + Period:sine_vol | Bat), # random slopes 
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 3000, warmup = 1200, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9, max_treedepth = 12),  
      seed = 12)


save(m.trends.obj, file = "m.trends.obj.3000iter.rda")
load("m.trends.obj.3000iter.rda")

summary(m.trends.subj)

save(m.trends.subj, file = "m.trends.subj.2000iter.rda")
load("m.trends.subj.2000iter.rda")

summary(m.trends.subj)

# m.trends.obj <-
#   brm(data = Trends_model_subj, family = binomial,
#       reward_status | trials(1) ~ Period + trend + sine_vol + 
#         trend: sine_vol +
#         (1 + Period + trend + sine_vol | Bat), # random slopes 
#       prior = c(prior(normal(0, 10), class = Intercept),
#                 prior(normal(0, 10), class = b),
#                 prior(cauchy(0, 1), class = sd)),
#       iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
#       control = list(adapt_delta = 0.9995, max_treedepth = 15),  
#       seed = 12)

#######
# Question 5: What is the effect of the fixed option and period on the 
# difference between up and down for preference for the fluctuating 
# option when its equal to the fixed option? The blue dot plot

Trend_2fix_diff <- Trend_2fix_diff %>% 
  mutate(Experiment = ifelse(Experiment == "Fl = 13.5", "Subjective", "Objective"))

Trend_fixdiff_model_subj <- Trend_2fix_diff %>% 
  ungroup() %>% 
  filter(Experiment == "Subjective") %>% 
  select(Period, IdLabel, updowndiff) %>% 
  rename(Bat = IdLabel) %>% 
  mutate(updowndiff = round(updowndiff, digits = 2))

Trend_fixdiff_model_obj <- Trend_2fix_diff %>% 
  ungroup() %>% 
  filter(Experiment == "Objective") %>% 
  select(Period, IdLabel, updowndiff) %>% 
  rename(Bat = IdLabel) %>% 
  mutate(updowndiff = round(updowndiff, digits = 2))

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

m.fixdiff.obj <-  
  brm(updowndiff ~ Period + (1 | Bat), data = Trend_fixdiff_model_obj, 
      family = student(), 
      prior = c(prior(normal(0, 1), class = "b"), 
                prior(student_t(4, 0, 2.5), class = "sd"), 
                prior(student_t(4, 0, 2.5), class = "sigma")), 
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 12)

save(m.fixdiff.subj, file = "m.fixdiff.obj.rda")
load("m.fixdiff.obj.rda")

