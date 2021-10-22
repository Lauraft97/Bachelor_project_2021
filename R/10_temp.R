# Loading weather data
load("data/raw_data/weather.rda")

library(tidyverse)

# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Column with dates and hours seperated
weather <- weather %>% select(location, date_time, temp, temp_max, temp_min, rain_1h, snow_1h) %>% 
  mutate(Date = format(date_time, format = "%d-%m-%Y"),
         Date = as.Date(Date, format = "%d-%m-%Y"),
         Time = format(date_time, format = "%H"),
         Time = as.numeric(Time)) 


# Only work with weather from 1. jan 2015 and forward
weather <- weather %>% filter(Date >= as.Date("2015-01-01"))

# Writing sine function to acount for ground vs air temperature
sine_ground_vs_air <- function(a_sub,a_add,x){
  #If between 8pm and 8am 
  if(x <= 8 | x> 20){
    #Sine funtion that will be used to add a maximum of 
    #"a_add" degrees to the air temperature
    sine(a=a_add, b=2*pi/24, c=-4, d=0, x=x)
  }
  else{
    #Sine funtion that will be used to subtract a maximum of 
    #"a_sub" degrees to the air temperature
    #Function between 8am and 8pm
    sine(a=a_sub, b=2*pi/24, c=-4, d=0, x=x)
  }
}

#Adding ground temperature variable by using the sine function 
weather <- weather %>% rowwise() %>% mutate(ground_temp = round(temp+sine_ground_vs_air(3.5,2,Time),2))

#Subtracting 10 degrees from the ground temp 
#(minimum for snail and parasite development)
#Setting values above 25 degrees and below 10 to 0
# In this case above 25 and below 0
weather <- weather %>% mutate(ground_temp_ten = ground_temp - 10,
                              ground_temp_ten = case_when(ground_temp_ten < 0 | ground_temp_ten > 15 ~ 0,
                                                           TRUE ~ ground_temp_ten))


#Finding days with above 10 and below 25 degrees, and summing 
# the hours that are above 10 and below 25 degrees
warm_days <- weather %>% group_by(location,Date) %>% 
  filter(ground_temp_ten > 0) %>% 
  summarise(n_hours = n())

#Going from hourly data to daily data.
# Summing the daily snow and rain
# Calculating the mean ground temperature
# Calculating the mean ground temperature with 10 subtracted
daily_weather <- weather  %>% 
  group_by(location,Date) %>% 
  summarise(rain = sum(rain_1h, na.rm = T),
            snow = sum(snow_1h, na.rm = T),
            mean_ground_temp = mean(ground_temp),
            mean_ground_temp_ten = mean(ground_temp_ten)) %>% 
  mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>% 
  arrange(Date) %>% 
  ungroup()

#Joining the daily weather data with the hours above 10 and below 25 degrees pr day
daily_weather <- full_join(daily_weather,warm_days,by=c("location","Date")) 

#Arranging by location and then date
daily_weather <- daily_weather %>% arrange(location,Date)

#Saving the daily weather file for later use in ODE
save(daily_weather,file = "data/10_model_weather.RData")


