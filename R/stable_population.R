rm(list = ls())

#set.seed(1234)
#set.seed(756)
set.seed(744)


library(tidyverse)
library(lubridate)
library(statmod)


# Import functions --------------------------------------------------------
source(file = "R/04x_cow_dynamics.R")
source(file ="R/99_functions.R")
#source(file = "R/11_sunrise_sunset_data.R")
#source(file = "R/10_ODE_rates.R")
source(file = "R/14_Farm_info.R")

Farm_info <- Farm_var("C1")

First_sample <- Farm_info[[1]]
nCows <- Farm_info[[4]]

# Time values

month5 <- 152 
month10 <- 304
year <- 365


# Parameters --------------------------------------------------------------
time <- 3*365
First_DOB <- First_sample - 3.75*year
date <- First_sample
nE0 <- 1
ID_no <- nCows

sla_prop <- 0.5

#Placeholder to fill out
Pop <- rep(0,time)
Births <- rep(0,time)


Farm <- tibble(CowID = 1:nCows,
               DOB = as.Date(x = rdunif(n = nCows,
                                        a = as.integer(First_DOB),
                                        b = as.integer(First_sample)),
                             origin = "1970-01-01"),
               Group = case_when(First_sample - DOB <= month10 ~ 1,
                                 First_sample - DOB > month10 & First_sample - DOB <= 2*year ~ 2,
                                 First_sample - DOB > 2*year ~ 3),
               Lactation = 0,
               State = 1,
               E_period = 0,
               I_period = 0,
               sick_period = 0,
               n_calfs = 0,
               cycle_day = 0,
               Grazing = 0,
               Age = as.numeric(First_sample-DOB))


Farm$Group <- factor(Farm$Group, levels=c(1:3))

E0_cows <- sample(1:nCows,
                  nE0,
                  replace = F)


Farm <- cow_pop_init(Farm)

Farm[22,10] <- 304


Pop[1] <- nCows


for(k in 2:time){
  
  
  date <- date + 1
  
  #Slaughter
  Farm <- Farm %>% mutate(slaughter = 
                            case_when(cycle_day == month10 ~ as.numeric(rbinom(1,1,sla_prop)),
                                      TRUE ~ 0))
  
  Farm <- Farm %>% 
    filter(!(slaughter == 1))
    
  
  # Count the number of cows who will have a calf
  Births[k] <- Farm %>% filter(Age == 2*year | cycle_day == year) %>% nrow()
  
  # Removing half or random if unequal number since some calf will be male
  if((Births[k] %% 2) != 0){
    Births[k] <- (Births[k] - 1)/2 + rbinom(1,1,0.5)  
  } else{
    Births[k] <- Births[k]/2 
  }
  
  #Updating ID 
  if(Births[k] > 0){
    ID_no <- ID_no + Births[k]
  } 
  
  # Moving through the different groups (cow population dynamics)
  # 04x_cow_dynamics.R
  Farm <- cow_dynamics(Farm)
  
  # Add calf to the population
  if(Births[k] > 0){
    new_calfs <- tibble(CowID = (ID_no+1-Births[k]):ID_no,
                        DOB = date,
                        Group = 1,
                        Lactation = NA,
                        State = 1,
                        E_period = 0,
                        I_period = 0,
                        sick_period = 0,
                        n_calfs = 0,
                        cycle_day = NA,
                        Grazing = runif(Births[k],0,0.1))
    
    Farm <- bind_rows(Farm,new_calfs)
  }
  
  Pop[k] <- Farm %>% nrow()
  print(time-k)  
  
}


ggplot(mapping = aes(x = 1:time,
                     y = Pop)) +
  geom_line()


ggplot(mapping = aes(x = 1:time,
                     y = Births)) +
  geom_line()







