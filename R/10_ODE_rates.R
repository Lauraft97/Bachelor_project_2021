# Loading weather data
load("data/raw_data/weather.rda")

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Column with dates without the time stamp
weather <- weather %>% select(location, date_time, temp, temp_max, temp_min, rain_1h, snow_1h) %>% 
                       mutate(Date = format(date_time, format = "%d-%m-%Y"),
                              Date = as.Date(Date, format = "%d-%m-%Y"),
                              Time = format(date_time, format = "%H")) 

# To work with one of the locations are removed
weather <- weather %>% filter(location == "Toender") %>% filter(Date >= as.Date("2015-01-01"))

# Assigning night/day to each time point
# Creating tibble with all data
weather <- full_join(weather, sun_data, by = "Date")

weather <- weather %>% mutate(Night_day = if_else(as.numeric(Time) < as.numeric(Sunrise) |
                                                  as.numeric(Time) > as.numeric(Sunset),
                                                  "Night", "Day"))

# For each day the maximum and minimum temperature and rain/snow (from 2015 and forward)
daily_weather <- weather %>% filter(Date >= as.Date("2015-01-01")) %>% 
  group_by(Date, Night_day) %>% 
  summarise(rain = sum(rain_1h, na.rm = T),
            snow = sum(snow_1h, na.rm = T),
            mean_temp = mean(temp)) %>% 
  mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>% 
  arrange(Date) %>% 
  ungroup()

# Each day is assigned a outcome for infection and death based on temperature
daily_weather <- daily_weather %>% mutate(Temp_case =
                                          case_when(mean_temp < 10 ~ 1,
                                                    mean_temp > 25 ~ 3,
                                                    TRUE ~ 2))





#Different scenarios for temperature

Rates <- function(date){
  
  #Filtering data for given day  
  DD <- daily_weather %>% filter(Date == date)
  
  #Both night and day temperature are below 10 celsius
  if(sum(DD$Temp_case) == 2){
    lambda_ES <- 0 #No snails get infected 
    mu_egg <- 0.6
    delta_snail <- runif(1,0.1,0.5)
  }
  
  if(sum(DD$Temp_case) == 3){
    DD <- DD %>% filter(mean_temp >= 10)
    lambda_ES <- log_growth(1,15,0.5,DD$mean_temp)*0.0000005 
    mu_egg <- (1-log_growth(1,15,0.5,DD$mean_temp))*0.6
    delta_snail <- runif(1,0.5,1.2)
  }
  
  if(sum(DD$Temp_case == 2) == 2){
    lambda_ES <- mean(log_growth(1,15,0.5,DD$mean_temp)*0.0000005) 
    mu_egg <- mean((1-log_growth(1,15,0.5,DD$mean_temp))*0.6)
    delta_snail <- runif(1,1.2,1.9)
  }
  
  if(sum(DD$Temp_case == 3) == 1 & sum(DD$Temp_case) == 4){
    DD <- DD %>% filter(mean_temp >= 10)
    lambda_ES <- 0 
    mu_egg <- (1-log_growth(1,15,0.5,DD$mean_temp))*0.6
    delta_snail <- runif(1,0.2,0.3)
     
  }
  
  if(sum(DD$Temp_case) == 5){
    mu_egg <- mean((1-log_growth(1,15,0.5,DD$mean_temp))*0.6)
    DD <- DD %>% filter(mean_temp <= 25)
    lambda_ES <- log_growth(1,15,0.5,DD$mean_temp)*0.0000005
    delta_snail <- runif(1,1.2,1.9)
  }
  
  
  Rates <- c(lambda_ES,mu_egg,delta_snail)
  return(Rates) 
  
}


