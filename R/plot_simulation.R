



# Validation vs data plot

rm(list=ls())

library(tidyverse)

# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")
load("data/Simulations_SSL/Cohort_sim_O2.RData")
source(file = "R/14_Farm_info.R")


# Initiation
Farm_info <- Farm_var("O2")


# Convert to factors ------------------------------------------------------

fluke_data <- fluke_data %>% mutate(Group = factor(x = Group,
                                                   levels = c("Calf",
                                                              "Heifer",
                                                              "Primiparous",
                                                              "Multiparous")))

# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm

fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE)) 

# Makes a tibble that contains the number of sick animals at visit B-G

fluke_data_inf <- fluke_diag %>% filter(Farm == "O2", Diag == T) %>% 
                  group_by(Visit) %>% summarise(n = n()) %>% slice(-1) 


#Makes a tibble that contains all the simulation data


sim_data <- bind_rows(sim, .id = "Simulation")


# Translates visit_day_no to visit B-G
visit_days_n <- Farm_info[[5]]


# Calculating the mean and the 95 % confidence interval for the data at each visit


sim_data <- sim_data %>% rename(inf_cows = n) %>% mutate(Visit = case_when(Visit_day_no == visit_days_n[1] ~ "B",
                                                                           Visit_day_no == visit_days_n[2] ~ "C",
                                                                           Visit_day_no == visit_days_n[3] ~ "D",
                                                                           Visit_day_no == visit_days_n[4] ~ "E",
                                                                           Visit_day_no == visit_days_n[5] ~ "F",
                                                                           Visit_day_no == visit_days_n[6] ~ "G"))


# Mean and confidence interval
results <- sim_data %>%
          group_by(Visit) %>%
          summarise(mean = mean(inf_cows, na.rm = TRUE),
                    sd = sd(inf_cows, na.rm = TRUE),
                    n = n()) %>%
          mutate(se = sd / sqrt(n),
                 lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                 upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

# Plot 1
results %>% ggplot(aes(x = Visit,
                       group = 1)) + 
            geom_line(aes(y = mean)) +
            geom_line(aes(y = lower.ci,
                          color = "lower CI"),
                          linetype = 2) +
            geom_line(aes(y = upper.ci,
                          color = "upper CI"),
                          linetype = 2) + 
  geom_point(aes(y = fluke_data_inf$n))



# Plot 2
ggplot(sim_data, aes(x = Visit,
                     y = inf_cows)) + 
geom_point(aes(color = Simulation)) + 
geom_point(data = fluke_data_inf, 
           aes(x = Visit,
               y = n),
           shape = 4)

