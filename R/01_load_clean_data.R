# 01 Load data
rm(list = ls())

library(tidyverse)


# Load data ---------------------------------------------------------------

fluke_data_raw <- read_csv(file = "data/raw_data/fluke_data.csv")


# Cleaning data -----------------------------------------------------------

# Converting Inf in LastTreatmentDays to NA

fluke_data <- fluke_data_raw %>% 
  mutate(LastTreatmentDays = na_if(LastTreatmentDays,Inf))

#Diagnostics converted to logical attributes

fluke_data <- fluke_data %>% mutate(dEPG = if_else(dEPG == "pos",
                                    TRUE,
                                    FALSE),
                                    dSerum = if_else(dSerum == "pos",
                                                   TRUE,
                                                   FALSE),
                                    dCopro = if_else(dCopro == "pos",
                                                   TRUE,
                                                   FALSE))

# Weather data --------------------------------------------------------------

weather <- load("data/raw_data/weather.rda")

# Write data --------------------------------------------------------------

#fluke
write_tsv(x = fluke_data,
          file = "data/01_fluke_data_clean.tsv")

#weather
write_tsv(x = weather,
          file = "data/01_weather_data.tsv")

