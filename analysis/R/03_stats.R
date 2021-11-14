#####
# Statistical analyses
#####
rm(list = ls())

require(tidyverse)

# Question 1: What do the bats match to? 

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

# Question 2: The effect of period and fixed output on matching 

# Question 3: What was the effect of period and fixed output on the choice 
# for the profitable option? 

# Question 4: What is the difference between the preference for the 
# fluctuating option as part of the upward and the downward trends as an effect of 
# fixed output, period and volume of the fluctuating option

# Question 5: What is the effect of the fixed option and period on the 
# difference between up and down for preference for the fluctuating 
# option when its equal to the fixed option? The blue dot plot



# Question 6: 
