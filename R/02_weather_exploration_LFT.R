rm(list = ls())

# Loading weather data
load("data/raw_data/weather.rda")

library(tidyverse)

# Column with dates without the timestamp
weather <- weather %>% mutate(Date = format(date_time, format = "%d-%m-%Y"))


# For each day the maximum and minimum temperature and rain/snow
temperature <- weather %>% group_by(location,Date) %>% 
               summarise(Day_max_temp = max(temp_max),
                         Day_min_temp = min(temp_min),
                         rain = sum(rain_1h, na.rm = T),
                         snow = sum(snow_1h, na.rm = T),
                         mean_temp = mean(temp)) %>% 
               mutate(Date = as.Date(Date, format = "%d-%m-%Y"),
                      Day_diff = Day_max_temp - Day_min_temp) %>% 
               arrange(location,Date) %>% 
               ungroup()

temperature %>% filter(rain > 0 | snow > 0)


# Plotting the mean temperature for each day
temperature %>% ggplot(mapping = aes(Date,
                                     mean_temp,
                                     col = location)) +
                geom_line()
  
# Plotting the maximum and minimum temperature for each location from 2015 and forward
temperature %>% filter(Date >= as.Date("2015-01-01")) %>% 
                ggplot(mapping = aes(x = Date)) +
                geom_line(aes(y = Day_max_temp,
                              col = "Max_temp")) +
                geom_line(aes(y = Day_min_temp,
                              col = "Min_temp")) +
                facet_wrap(~location)

# Days with precipitation (2015 and forward)
precipitation <- temperature %>% filter(Date >= as.Date("2015-01-01") & (rain != 0 | snow != 0)) 

# Monthly averages of rain and snow
rain_snow <- precipitation %>% mutate(month_yr = format(Date, format = "%Y-%m")) %>% 
                               group_by(location,month_yr) %>% 
                               summarise(Mon_rain = mean(rain),
                                         Mon_snow = mean(snow)) %>% 
                               arrange(location,month_yr)
  
                


