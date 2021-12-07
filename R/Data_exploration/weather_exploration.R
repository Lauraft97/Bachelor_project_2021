# 02 Weather exploration
rm(list = ls())

library(tidyverse)


# Load data ---------------------------------------------------------------
load("data/raw_data/weather.rda")


# Statistics --------------------------------------------------------------
#Variables
names(weather)

# Dimensions
weather %>% 
  summarise("Number of timestamps" = n(),
            "Number of variables" = ncol(.))

#locations
weather %>% distinct(location)

#First and last date and time point
weather %>% arrange(date_time) %>% 
  select(date_time) %>% 
  filter(row_number() == 1 | row_number() == n())

#First data point: 1st of Jan 1979 at 00:00
#Last data point: 19th of Sep 2021 at 23:00

#make seperate column for year
weather <- weather %>% 
  rowwise() %>% 
  mutate(year = format(date_time,format = "%Y")) %>% 
  select(location, date_time, year, everything())

#plot temperature for the first year
weather %>% filter(year == "1979") %>%
  ggplot(mapping = aes(x = date_time,
                       y = temp,
                       color = year)) +
  geom_line()  
#facet_wrap(~ year)

#plot temperature for year 1779-1985
weather %>% filter(year <= "1985") %>%
  ggplot(mapping = aes(x = date_time,
                       y = temp,
                       color = year)) +
  geom_line() 
 



       