#08_Cow_dynamics
rm(list = ls())

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")


# fluke_data %>% arrange(desc(DOB)) %>% 
#   filter(DOB < Date)


# Parameters --------------------------------------------------------------
nCows <- 300
#nE0 <- 3
time <- 365*3
#Mcer <- 10^4

# Creating data frame of susceptible cows.
# DOB from 2008 to 2014 (looking in data for visit A)
#Start date for full data: 27. Apr 2015

Start.date <- as.Date("2015-04-27")

# Farm <- tibble(CowID = 1:nCows,
#                DOB = as.Date(x = rdunif(n = nCows,
#                                         a = as.integer(as.Date("2008-11-15")),
#                                         b = as.integer(as.Date("2014-10-24"))),
#                              origin = "1970-01-01"),
#                Group = case_when(Start.date - DOB <= 10*30 ~ "1",
#                                  Start.date - DOB > 10*30 & Start.date - DOB <= 2*365 ~ "2",
#                                  Start.date - DOB > 2*365 & Start.date - DOB <= 3*365 ~ "3",
#                                  Start.date - DOB > 3*365 ~ "4"),
#                State = 1,
#                E_period = 0,
#                I_period = 0,
#                Grazing = runif(nCows,0.4,0.6))


#Farm$Group <- factor(Farm$Group, levels=c(1:4))

Farm <- tibble(CowID = 1:nCows,
               Group = rep(1:4,75),
               DOB = case_when(Group == 1 ~ as.Date(rdunif(n = nCows,
                                                           a = as.integer(as.Date(Start.date)-10*30),
                                                           b = as.integer(as.Date(Start.date))),
                                                    origin = "1970-01-01"),
                               Group == 2 ~ as.Date(rdunif(n = nCows,
                                                           a = as.integer(as.Date(Start.date)-2*365),
                                                           b = as.integer(as.Date(Start.date)-10*30)),
                                                    origin = "1970-01-01"),
                               Group == 3 ~ as.Date(rdunif(n = nCows,
                                                           a = as.integer(as.Date(Start.date)-3*365),
                                                           b = as.integer(as.Date(Start.date)-2*365)),
                                                    origin = "1970-01-01"),
                               Group == 4 ~ as.Date(rdunif(n = nCows,
                                                           a = as.integer(as.Date(Start.date)-5*365),
                                                           b = as.integer(as.Date(Start.date)-3*365)),
                                                    origin = "1970-01-01")),
               State = 1,
               E_period = 0,
               I_period = 0,
               Grazing = runif(nCows,0.4,0.6))

date <- Start.date
for(k in 2:time){
  #Moving through groups
  Farm <- Farm %>% mutate(Group = 
                            case_when(date - DOB <= 10*30 ~ 1,
                                      date - DOB > 10*30 & date - DOB <= 2*365 ~ 2,
                                      date - DOB > 2*365 & date - DOB <= 3*365 ~ 3,
                                      date - DOB > 3*365 ~ 4))
  
  #Slaughter
  #Here removing cows more then 4 years old
  Farm <- Farm %>% 
    filter(DOB > date - 4*365)
  
  
  date = date + 1
}


# No stocastisity
# Uniform distribution in slaughter
# Births: When heifer = 2 years
# Undersøge hvor mange der slagtes, hvor mange skal så fødes for at holde en 
# "konstant population"




