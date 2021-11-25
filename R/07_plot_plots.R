rm(list=ls())

library(tidyverse)
library(patchwork)

First_sample <- as.Date("2015-04-27")


# Color_schemes -----------------------------------------------------------
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
# color_df <- as.data.frame(cbind(var = c("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2"),
#                                  color_scheme))
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]



# Loading ODE result dataframes -------------------------------------------
load("results/Eggs.RData")
load("results/Snails.RData")
load("results/Metacercariae.RData")


# Egg plots --------------------------------------------------------------

# Eggs on each farm (together)
Eggs %>% filter(variable == "Egg") %>% ggplot(aes(x = date,
                                                  y = median,
                                                  color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 


# New eggs on each farm (together)
Eggs %>% filter(variable == "new_Egg") %>% ggplot(aes(x = date,
                                                  y = median,
                                                  color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Eggs [#]",
       title = "New excreted eggs",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 

# Eggs on a single farm - Q1, median and Q3 (no limit on time)
# Remember to make sure farm and color corresponds
Eggs %>% filter(variable == "Egg" & Farm == "O2") %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_line(aes(y = Q1, color = "Quantiles"),
            alpha = 0.5,
            linetype = "dashed") +
  geom_line(aes(y = Q3, color = "Quantiles"),
            alpha = 0.5,
            linetype = "dashed") + 
  scale_color_manual(values = rep(color_scheme[8],3)) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm O2",
       subtitle = "Q1, median and Q3 of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 

# Eggs on a single farm - Q1, median and Q3 (limit on time to after July 2016
# Remember to make sure farm and color corresponds
Eggs %>% filter(variable == "Egg" & Farm == "O1" & date > as.Date("2016-07-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_line(aes(y = Q1, color = "Quantiles"),
            alpha = 0.5,
            linetype = "dashed") +
  geom_line(aes(y = Q3, color = "Quantiles"),
            alpha = 0.5,
            linetype = "dashed") + 
  scale_color_manual(values = rep(color_scheme[6],3)) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm O2",
       subtitle = "Q1, median and Q3 of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = as.Date("2016-07-01"), 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 




# Snail plots -------------------------------------------------------------
# All of the four farm for E1
Snails %>% filter(variable == "E1") %>% ggplot(aes(x = date,
                                                      y = median,
                                                      color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Exposed state 1 snails",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 

# All of the four farm for E2
Snails %>% filter(variable == "E2") %>% ggplot(aes(x = date,
                                                   y = median,
                                                   color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Exposed state 2 snails",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 

# All of the four farm for I
Snails %>% filter(variable == "I") %>% ggplot(aes(x = date,
                                                   y = median,
                                                   color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Infected snails",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 

# Exposed state 1 and 2 and infected snails 
Snails %>% filter(Farm == "O1" & variable %in% c("E1","E2","I")) %>% 
  ggplot(aes(x = date,
             y = median,
             color = variable)) +
  geom_line(alpha = 0.6) + 
  scale_color_manual(values=c(color_scheme_2[4], 
                              color_scheme_2[8], 
                              color_scheme_2[10]),
                     name = "State") + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Exposed and infected snails on O1",
       subtitle = "Median of 10 simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") 






Metacercariae %>% ggplot(aes(x = date,
                            y = median,
                            color = Farm)) +
  geom_line()

