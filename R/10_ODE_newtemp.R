# Loading weather data
load("data/raw_data/weather.rda")

library(tidyverse)

# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Column with dates without the time stamp
weather <- weather %>% select(location, date_time, temp, temp_max, temp_min, rain_1h, snow_1h) %>% 
  mutate(Date = format(date_time, format = "%d-%m-%Y"),
         Date = as.Date(Date, format = "%d-%m-%Y"),
         Time = format(date_time, format = "%H"),
         Time = as.numeric(Time)) 


# To work with one of the locations are removed
weather <- weather %>% filter(location == "Toender") %>% filter(Date >= as.Date("2015-01-01"))

# Adding variable to describe ground level temp using sine function

sine_2 <- function(a_sub,a_add,x){
  if(x <= 8 | x> 20){
    sine(a=a_add, b=2*pi/24, c=-4, d=0, x=x)
  }
  else{
    sine(a=a_sub, b=2*pi/24, c=-4, d=0, x=x)
  }
}

weather <- weather %>% rowwise() %>% mutate(ground_temp = temp+sine_2(3.5,2,Time))

# weather %>% filter(Date == as.Date("2021-01-10")) %>% 
#   ggplot(mapping = aes(x=date_time)) +
#   geom_line(aes(y = temp)) +
#   geom_line(aes(y = ground_temp,
#                 col = "red")) +
#   geom_line(aes(y = 25)) +
#   geom_line(aes(y = 10))

weather <- weather %>% mutate(ground_temp_upda = ground_temp - 10,
                              ground_temp_upda = case_when(ground_temp_upda < 0 | ground_temp_upda > 15 ~ 0,
                                                           TRUE ~ ground_temp_upda))


warm_days <- weather %>% group_by(Date) %>% 
  filter(ground_temp_upda > 0) %>% 
  summarise(n = n())

daily_weather <- weather  %>% 
  group_by(Date) %>% 
  summarise(rain = sum(rain_1h, na.rm = T),
            snow = sum(snow_1h, na.rm = T),
            mean_ground_temp = mean(ground_temp),
            mean_ground_temp_ten = mean(ground_temp_upda)) %>% 
  mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>% 
  arrange(Date) %>% 
  ungroup()

daily_weather <- full_join(daily_weather,warm_days,by="Date")

# daily_weather %>% filter(Date <= as.Date("2016-01-01")) %>% 
#   ggplot(mapping = aes(x = Date, 
#                        y = mean_ground_temp_ten)) +
#   geom_line()
# 
# plot(pexp(q = c(1:14),rate = 0.25))
# 
# daily_weather  %>% filter(mean_ground_temp_ten > 0) %>% 
#   ggplot(mapping = aes(mean_ground_temp_ten)) +
#   geom_histogram()
# 
# pexp(q = 0,rate = 0.5)


# Defining the maximum rates 
lambda_ES_max <- 0.0000005 #Transmission rate egg to snail
mu_Egg_max <- 0.65 # Death rate eggs (become non-infectious)
delta_snail_max <- 1.5 #Daily snail population "scaling factor"
gamma_S_max <- 2 #Excretion of metacercarria from snail
mu_S_max <- 0.05 #Death rate of infected snails / recvoery rate
mu_M_max <- 0.15 # Death rate of metacercaria
  
  
  
  
  
Rates <- function(date){
  #Filtering data for given day  
  DD <- daily_weather %>% filter(Date == date) %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
  lambda_ES <- pexp(q = DD,rate = 0.25)*lambda_ES_max
  mu_Egg <- (1-pexp(q = DD,rate = 0.25))*mu_Egg_max
  delta_snail <- pexp(q = DD,rate = 0.25)*delta_snail_max
  mu_M <- (1-pexp(q = DD,rate = 0.25))*mu_M_max
  
  Rates <- c(lambda_ES,mu_Egg,delta_snail,mu_M)
  return(Rates) 
  
}

