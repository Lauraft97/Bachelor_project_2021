# Plots included in the appendixes
library(tidyverse)
library(patchwork)
library(cowplot)

# Plot parameters ---------------------------------------------------------
First_sample <- as.Date("2015-04-27")
windowsFonts(`Lucida Bright` = windowsFont("Lucida Bright"))
Sys.setlocale("LC_ALL","English")

# Color_schemes 
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]
# Colors: ("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2")


# Plot function for SEI group plots
plot_SEI <- function(FarmID,nruns){
  
  library(tidyverse)
  library(patchwork)
  
  source(file = "R/00B_farm_info.R")
  
  load(paste0("results/IBM_",FarmID,".Rdata"))
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  First_sample <- Farm_var(FarmID)[[1]]
  time <- as.integer(as.Date("2017-12-31")-First_sample)
  last_visit <- First_sample + Farm_var(FarmID)[[5]][[6]]
  
  
  
  # Create dataframes instead of lists-------------------
  IBM_S <- bind_rows(sapply(sim_IBM, "[", 1))
  IBM_E <- bind_rows(sapply(sim_IBM, "[", 2))
  IBM_I <- bind_rows(sapply(sim_IBM, "[", 3))
  
  
  # Calculate mean and confidence intervals -------------
  results_IBM_S <- IBM_S %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("S"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group,date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75") 
  
  results_IBM_E <- IBM_E %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("E"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group, date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75")
  
  results_IBM_I <- IBM_I %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("I"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group, date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75")
  
  
  #Plot -------------------------------------------
  S <- results_IBM_S %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Susceptible") +
    theme(axis.text.x=element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0), "cm"))+ 
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + ylim(0,170)
  
  
  E <- results_IBM_E %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Exposed") +
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + 
    theme(axis.text.x=element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"))
  
  
  I <- results_IBM_I %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "Date",
         y = "Cattle [#]",
         title = "Infected") +
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin=unit(c(0,0,0,0), "cm"))
  
  p <- S/E/I + plot_annotation(title = paste("Farm", FarmID, sep = " "),
                               subtitle = "IBM - Median of simulations") & 
    theme(plot.title = element_text(family = "Lucida Bright"),
          plot.subtitle = element_text(family = "Lucida Bright"))
  
  
  # Return from function ----------------
  
  return(p)
}



# SEI group plot C2 and O2 ------------------------------------------------
plot_SEI("C2",30)
ggsave("results/figures/appendix/SEI_group_C2.png")

plot_SEI("O2",30)
ggsave("results/figures/appendix/SEI_group_O2.png")







# Egg quartile plot C1, C2 and O2  -------------------------------------------
load("results/Eggs.RData")

Eggs %>% filter(variable == "Egg" & Farm == "C1" & 
                  date > as.Date("2016-11-01") & 
                  date < as.Date("2017-11-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Quartiles"), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = color_scheme[4],
                     name = "") + 
  scale_fill_manual(values = color_scheme[4],
                    name = "") + 
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm C1") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2016-11-01"), 
                                               max = as.Date("2017-11-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_Eggs_quantiles_C1.png")

Eggs %>% filter(variable == "Egg" & Farm == "C2" & 
                  date > as.Date("2016-11-01") & 
                  date < as.Date("2017-11-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Quartiles"), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = color_scheme[5],
                     name = "") + 
  scale_fill_manual(values = color_scheme[5],
                    name = "") + 
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm C2") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2016-11-01"), 
                                               max = as.Date("2017-11-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_Eggs_quantiles_C2.png")

Eggs %>% filter(variable == "Egg" & Farm == "O2" & 
                  date > as.Date("2016-11-01") & 
                  date < as.Date("2017-11-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "Median")) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Quartiles"), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = color_scheme[8],
                     name = "") + 
  scale_fill_manual(values = color_scheme[8],
                    name = "") + 
  labs(x = "Date",
       y = "Eggs [#]",
       title = "Eggs on farm O2") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2016-11-01"), 
                                               max = as.Date("2017-11-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_Eggs_quantiles_O2.png")








# Daily snail population and suscptible C2, O1 and O2 -------------------------------
load("results/Snails.RData")

Snails %>% filter(Farm == "C2" & variable %in% c("pop","S")) %>% 
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
       title = "Farm C2: Daily snail population and susceptibles",
       subtitle = "Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_pop_S_C2.png")

Snails %>% filter(Farm == "O1" & variable %in% c("pop","S")) %>% 
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
       title = "Farm O1: Daily snail population and susceptibles",
       subtitle = "Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_pop_S_O1.png")

Snails %>% filter(Farm == "O2" & variable %in% c("pop","S")) %>% 
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
       title = "Farm O2: Daily snail population and susceptibles",
       subtitle = "Median of simulations") +
  scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "results/figures/appendix/ODE_pop_S_O2.png")










# Snail exposed / infected for C2 and O2 ----------------------------------
C2_EI <- Snails %>% filter(Farm == "C2" & variable %in% c("E1","E2","I")) %>% 
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
       subtitle = "Farm C2; ODE - Median of simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y")  +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


O2_EI <- Snails %>% filter(Farm == "O2" & variable %in% c("E1","E2","I")) %>% 
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
       subtitle = "Farm O2; ODE - Median of simulations") +
  scale_x_date(breaks = "4 months", limits = c(min = First_sample, 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y")  +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


legend <- Snails %>% filter(Farm == "C2" & variable %in% c("E1","E2","I")) %>% 
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


plot_EI <- plot_grid(C2_EI,O2_EI)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot_grid(plot_EI, plot_legend,
          ncol = 1,
          rel_heights = c(15,1))


ggsave(filename = "results/figures/appendix/ODE_EI_C2O2.png")










# Infected snails median and quartiles C2 and O2 --------------------------
Snails  %>% filter(variable == "I" & (Farm == "C2" | Farm == "O2"),
                   date > as.Date("2017-06-01") & 
                     date < as.Date("2017-10-01")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = Farm)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = Farm), alpha =  0.3) + 
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(values = c(color_scheme[5],
                                color_scheme[8]),
                     name = "Median") + 
  scale_fill_manual(values = c(color_scheme[5],
                               color_scheme[8]),
                    name = "Quartiles") + 
  labs(x = "Date",
       y = "Snails [#]",
       title = "Infected snails on farm C2 and O2",
       subtitle = "ODE - Q1, median and Q3 of simulations") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2017-06-01"), 
                                               max = as.Date("2017-10-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) 

ggsave(filename = "results/figures/appendix/ODE_I_quartiles_C2O2_.png")













# Death rate eggs + weather -----------------------------------------------

load("results/mu_egg_data.RData")
load("data/10_model_weather.RData")

mu_egg %>% filter(Dates > as.Date("2016-11-01") & 
                  Dates < as.Date("2017-11-01")) %>% 
  ggplot(aes(x = Dates,
             y = mu)) + 
  geom_line(color = color_scheme[1],
            size = 1) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = expression(mu[egg]),
       title = "Death rate of eggs on O1") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2016-11-01"), 
                                               max = as.Date("2017-11-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("rect",
    xmin = as.Date("2017-01-15"),
           xmax = as.Date("2017-02-25"),
           ymin = 0,
           ymax = 1,
           fill = color_scheme_2[5],
           alpha = 0.3) 
ggsave("results/figures/appendix/mu_egg.png")


daily_weather %>% filter(Date > as.Date("2017-01-01") & 
                         Date < as.Date("2017-03-01") &
                         location == "Toender") %>% 
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = rain,
                color = "Rain"),
            size = 1) +
  geom_point(aes(y = mean_ground_temp_ten,
                color = "Corrected ground temperature")) +
  scale_color_manual(values = c(color_scheme_2[2],
                                color_scheme_2[8]),
                     name = "") + 
  theme_bw(base_size = 11,
           base_family = "Lucida Bright") +
  labs(x = "Date",
       y = "Rain [mm]",
       title = "Weather variables January - March 2017",
       subtitle = "Location: Tønder") +
  scale_x_date(breaks = "1 months", limits = c(min = as.Date("2017-01-01"), 
                                               max = as.Date("2017-03-01")),
               date_labels = "%b-%y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE), 
                       sec.axis = sec_axis(~ ., name = "[C\u00B0]"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.right = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom") +
  annotate("rect",
           xmin = as.Date("2017-01-15"),
           xmax = as.Date("2017-02-22"),
           ymin = 0,
           ymax =15,
           fill = color_scheme_2[5],
           alpha = 0.3) 

ggsave("results/figures/appendix/weather_egg.png")






