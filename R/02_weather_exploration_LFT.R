rm(list = ls())

# Loading weather data
load("data/raw_data/weather.rda")

library(tidyverse)

# Column with dates without the timestamp
weather <- weather %>% mutate(Date = format(date_time, format = "%d-%m-%Y"),
                              Date = as.Date(Date, format = "%d-%m-%Y"))


# For each day the maximum and minimum temperature and rain/snow (from 2015 and forward)
temperature <- weather %>% filter(Date >= as.Date("2015-01-01")) %>% 
               group_by(location,Date) %>% 
               summarise(Day_max_temp = max(temp_max),
                         Day_min_temp = min(temp_min),
                         rain = sum(rain_1h, na.rm = T),
                         snow = sum(snow_1h, na.rm = T),
                         mean_temp = mean(temp)) %>% 
               mutate(Date = as.Date(Date, format = "%d-%m-%Y"),
                      Day_diff = Day_max_temp - Day_min_temp) %>% 
               arrange(location,Date) %>% 
               ungroup()

# For the period relevant for our simulation the difference in mean and median temperatures are examined
period <- weather %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2015-03-01")) %>% 
  group_by(location,Date) %>% 
  summarise(Day_max_temp = max(temp_max),
            Day_max_med = median(temp_max),
            Day_max_mean = mean(temp_max),
            Day_min_temp = min(temp_min),
            Day_min_mean = mean(temp_min),
            rain = sum(rain_1h, na.rm = T),
            snow = sum(snow_1h, na.rm = T),
            mean_temp = mean(temp),
            median_temp = median(temp)) %>% 
  mutate(Date = as.Date(Date, format = "%d-%m-%Y"),
         Day_diff = Day_max_temp - Day_min_temp) %>% 
  arrange(location,Date) %>% 
  ungroup()

period %>% filter(location == "Frederikssund") %>% 
           ggplot(aes(x = 1:60)) +
  geom_line(aes(y = Day_max_med,
                col = "Median")) +
  geom_line(aes(y = Day_max_mean,
                col = "Mean")) +
  geom_line(aes(y = Day_max_temp,
                col = "Max"))


period %>% filter(location == "Frederikssund") %>% 
  ggplot(aes(x = 1:60)) +
  geom_line(aes(y = median_temp,
                col = "Median")) +
  geom_line(aes(y = mean_temp,
                col = "Mean")) +
  geom_line(aes(y = Day_max_mean,
                col = "max_Mean")) +
  geom_line(aes(y = Day_min_mean,
                col = "min_Mean")) 




# Days with precipitation
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
  
                


