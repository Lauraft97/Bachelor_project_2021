# ODE results plot

rm(list=ls())

library(tidyverse)
library(patchwork)
library(cowplot)

First_sample <- as.Date("2015-04-27")
windowsFonts(`Lucida Bright` = windowsFont("Lucida Bright"))
Sys.setlocale("LC_ALL","English")

color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
# color_df <- as.data.frame(cbind(var = c("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2"),
#                                  color_scheme))
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]



# New eggs excreted per day -----------------------------------------------
load("results/Eggs.RData")

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
       title = "Excreted eggs per day",
       subtitle = "IBM - Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename = "results/figures/Final_figures/ODE_new_eggs.png")






# Eggs --------------------------------------------------------------------
load("results/Eggs.RData")
Eggs %>% filter(variable == "Egg") %>% mutate(median = median/(10^6)) %>% 
  ggplot(aes(x = date,
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
       subtitle = "ODE - Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = c(0,"2M", "4M", "6M", "8M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename = "results/figures/Final_figures/ODE_Eggs.png")






# Eggs O1 median and quartiles --------------------------------------------
load("results/Eggs.RData")
Eggs %>% filter(variable == "Egg" & Farm == "O1" & 
                  date > as.Date("2016-11-01") & 
                  date < as.Date("2017-11-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Quantiles"), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = color_scheme[6],
                     name = "") + 
  scale_fill_manual(values = color_scheme[6],
                    name = "") + 
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm O1",
       subtitle = "ODE - Q1, median and Q3 of simulations") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2016-11-01"), 
                                               max = as.Date("2017-11-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/Final_figures/ODE_Eggs_quantiles_O1.png")







# Snail population and susceptibles on O1 ---------------------------------
load("results/Snails.RData")
Snails %>% filter(Farm == "C1" & variable %in% c("pop","S")) %>% 
  ggplot(aes(x = date,
             y = median,
             color = variable)) +
  geom_line(alpha = 0.6) + 
  scale_color_manual(values=c(color_scheme_2[2],
                              color_scheme_2[6]),
                     name = "",
                     label = c("Population","Susceptible")) + 
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Daily snail population and susceptibles on farm C1",
       subtitle = "ODE - Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/Final_figures/ODE_pop_S.png")













# Snails exposed and infected on all four farms ---------------------------------
load("results/Snails.RData")

E1 <- Snails %>% filter(variable == "E1") %>% ggplot(aes(x = date,
                                                         y = median,
                                                         color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "",
       y = "Snails [#]",
       title = "ODE snails - Median of simulations",
       subtitle = "Exposed state 1") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# All of the four farm for E2
E2 <- Snails %>% filter(variable == "E2") %>% ggplot(aes(x = date,
                                                         y = median,
                                                         color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "",
       y = "",
       title = "",
       subtitle = "Exposed state 2") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# All of the four farm for I
I <- Snails %>% filter(variable == "I") %>% ggplot(aes(x = date,
                                                       y = median,
                                                       color = Farm)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[4], 
                              color_scheme[5], 
                              color_scheme[6],
                              color_scheme[8])) +
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       subtitle = "Infected") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) 

(E1+E2)/I

ggsave(filename = "results/figures/Final_figures/ODE_snail_EI.png")














# Snails exposed and infected on O1 and C1 ---------------------------------
load("results/Snails.RData")

C1_EI <- Snails %>% filter(Farm == "C1" & variable %in% c("E1","E2","I")) %>% 
  ggplot(aes(x = date,
             y = median,
             color = variable)) +
  geom_line(alpha = 0.6) + 
  scale_color_manual(values=c(color_scheme_2[4], 
                              color_scheme_2[8], 
                              color_scheme_2[10]),
                     name = "State") + 
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Snails [#]",
       title = "Exposed and infected snails",
       subtitle = "Farm C1; ODE - Median of simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y")  +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


O1_EI <- Snails %>% filter(Farm == "O1" & variable %in% c("E1","E2","I")) %>% 
  ggplot(aes(x = date,
             y = median,
             color = variable)) +
  geom_line(alpha = 0.6) + 
  scale_color_manual(values=c(color_scheme_2[4], 
                              color_scheme_2[8], 
                              color_scheme_2[10]),
                     name = "State") + 
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "",
       title = "",
       subtitle = "Farm O1; ODE - Median of simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y")  +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


legend <- Snails %>% filter(Farm == "C1" & variable %in% c("E1","E2","I")) %>% 
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
       title = "Exposed and infected snails on farm C1",
       subtitle = "ODE - Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y")  +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


plot_EI <- plot_grid(C1_EI,O1_EI)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot_grid(plot_EI, plot_legend,
          ncol = 1,
          rel_heights = c(15,1))


ggsave(filename = "results/figures/Final_figures/ODE_EI_C1O1.png")





# Snails infected on O1 and C1 median and quartiles ------------------------
load("results/Snails.RData")

Snails  %>% filter(variable == "I" & (Farm == "C1" | Farm == "O1"),
                   date > as.Date("2017-06-01") & 
                     date < as.Date("2017-10-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = Farm)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = Farm), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = c(color_scheme[4],
                                color_scheme[6]),
                     name = "Median") + 
  scale_fill_manual(values = c(color_scheme[4],
                               color_scheme[6]),
                    name = "Quantiles") + 
  labs(x = "Date",
       y = "Snails [#]",
       title = "Infected snails on farm C1 and O1",
       subtitle = "ODE - Q1, median and Q3 of simulations") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2017-06-01"), 
                                               max = as.Date("2017-10-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) 

ggsave(filename = "results/figures/Final_figures/ODE_I_quantiles_C1O1_.png")







# Metacercariae   ------------------------
load("results/Metacercariae.RData")

Metacercariae %>% ggplot(aes(x = date,
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
       y = "Metacercariae [#]",
       title = "Metacercariae",
       subtitle = "ODE - Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/Final_figures/ODE_M.png")
