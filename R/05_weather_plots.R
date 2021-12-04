rm(list = ls())

#Import functions, data and packages
load("data//10_model_weather.RData")

library(tidyverse)
library(lubridate)
library(patchwork)
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Plot of sine function on 1 day---------------------------------------------------
#Sys.setlocale("LC_TIME", "C")
x <- c(0:23)
y <- rep(0,24)

for (i in x){
  y[i+1] <- sine_ground_vs_air(3.5,2,i)
}

ggplot(mapping = aes(x = x,
                     y = y)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray60") +
  geom_point(col = color_scheme[1], size = 2.5) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(title = "Sine function",
       subtitle = "Function to correct for differences between air and ground temperature",
       x = "Time [h]",
       y = "Degrees to add") +
  scale_x_continuous(name = "Hour of the day",
                     breaks = seq(0,23,1),
                     labels = c("midnight","01","02","03","04","05","06","07",
                                "08","09","10","11","noon","13","14","15","16",
                                "17","18","19","20","21","22","23")) +
  scale_y_continuous(name = "Degrees to add",
                     breaks = seq(-4,2,1)) +
  theme(axis.title.y = element_text(vjust=1),
        axis.text.x = element_text(vjust = 1, hjust=1, angle=45))

ggsave(filename = "results/figures/Final_figures/05_sine_function.png")  

# Plot of corrected temperature -----------------------------------------------------
break.vec <- c(as.Date("2015-01-01"),
               seq(from = as.Date("2015-04-01"), to = as.Date("2017-12-01"),
                   by = "3 months"),
               as.Date("2018-01-01"))

daily_weather %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31")) %>% 
  ggplot(mapping = aes(x = Date,
                       y = mean_ground_temp_ten)) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright")+
  geom_line(col = color_scheme[1])+
  labs(y = "Corrected ground temperature")+
  scale_x_date(breaks = break.vec, date_labels = "%b-%y")+
  theme(axis.text.x = element_text(vjust = 1, hjust=1, angle=45))+
  facet_wrap(~location, labeller = as_labeller(c("Frederikssund" = "Frederikssund", 
                                                 "Toender" = "Tønder")),
             nrow = 2)

ggsave(filename = "results/figures/Final_figures/05_corr_temp.png") 

# Rain plot and exploration -----------------------------------------------
First_sample <- as.Date("2015-01-01")
rain_toender <- rep(0,979)
rain_frederikssund <- rep(0,979)

for(i in c(1:length(rain_toender))){
  
  date <- First_sample + i
  
  last10Days <- date - 0:9
  
  rain_toender[i] <- daily_weather %>% filter(Date %in% last10Days,location == "Toender") %>%
    summarise(rain = sum(rain)) %>% pull()
  
  rain_frederikssund[i] <- daily_weather %>% filter(Date %in% last10Days,location == "Frederikssund") %>%
    summarise(rain = sum(rain)) %>% pull()
}

rain <- tibble(rain_toender, rain_frederikssund)

p_toender <- rain %>% ggplot(aes(x = 1:length(rain_frederikssund))) +
  geom_line(aes(y = rain_toender, col = "Tønder"))+
  geom_hline(yintercept = 2, linetype = "dashed", col = "gray60") +
  labs(x = "days",
       y = "rain [mm]",
       title = "Tønder") +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  theme(legend.position = "none")+ 
  coord_cartesian(ylim=c(0, 25))

p_frederikssund <- rain %>% ggplot(aes(x = 1:length(rain_frederikssund))) +
  geom_line(aes(y = rain_frederikssund, col = "Frederikssund")) +
  geom_hline(yintercept = 2, linetype = "dashed", col = "gray60") +
  labs(x = "days",
       y = "rain [mm]",
       title = "Frederikssund") +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  theme(legend.position = "none") + 
  coord_cartesian(ylim=c(0, 25))

p_toender / p_frederikssund

ggsave(filename = "results/figures/05_rain.png")



