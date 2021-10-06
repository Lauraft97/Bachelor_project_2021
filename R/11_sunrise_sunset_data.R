library(tidyverse)
library(readxl)
library(lubridate)

# Loading data
sun_data <- read_xlsx("data/raw_data/Sunrise_sunset_2015-2021.xlsx")

sun_data <- sun_data %>% select(Date, Sunrise, Sunset) %>% 
            mutate(Date = as.Date(Date, format = "%d-%m-%Y"),
                   Sunrise = format(round_date(Sunrise,"hour"), format = "%H"),
                   Sunset = format(round_date(Sunset,"hour"), format = "%H"))



