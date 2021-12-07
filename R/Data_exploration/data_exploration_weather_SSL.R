# 02 Data exploration

rm(list = ls())

load("/Users/sofielarsen/Downloads/weather.rda")
library(tidyverse)


# Variables names ---------------------------------------------------------
names(weather)
head(weather[1:9])
head(weather[10:18])


# Statistics --------------------------------------------------------------
#Locations
weather %>% distinct(location) # 2 locations: Frederikssund (C1) and Toender (C2, O1, O2)

#First and last date and time point
weather %>% arrange(date_time) %>% 
  select(date_time) %>% 
  filter(row_number() == 1 | row_number() == n())

#First data point: 1st of Jan 1979 at 00:00
#Last data point: 19th of Sep 2021 at 23:00

#Is there a data point every hour?
diff_time <- tibble(location = weather$location[1:nrow(weather)-1],
                    date_time = weather$date_time[1:nrow(weather)-1],
                    diff = as.numeric(diff(weather$date_time)))

diff_time %>% filter(diff != 3600)
# 13 data points have 2 datapoint for same hour

#Real dataset without dublicates:
#weather %>% distinct(location,date_time,.keep_all = TRUE)

# Time zone
weather %>% group_by(timezone) %>% 
  summarise(n = n())
  
# 2 time zones (Shift in seconds from UTC)

#Temperature
summary(weather$temp)

#Missing data?
weather[is.na(weather$temp),]

#Rain
summary(weather$rain_1h)

#Snow
summary(weather$snow_1h)

#Clouds, in %
summary(weather$clouds_all)

#Weather_ID
weather %>% group_by(weather_id) %>% 
  summarise(n = n())


# Plots -------------------------------------------------------------------

#Temperature First year
weather %>% 
  filter(location == "Frederikssund",
         date_time < ymd("1980-01-01")) %>% 
  ggplot(mapping = aes(x = date_time,
                       y = temp)) +
  geom_line()

weather %>% 
  ggplot(mapping = aes(x = location,
                       y = temp)) +
  geom_boxplot() 


