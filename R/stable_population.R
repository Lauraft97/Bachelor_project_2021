rm(list = ls())

#set.seed(1234)
#set.seed(756)
#set.seed(744)


library(tidyverse)
library(lubridate)
library(statmod)


# Import functions --------------------------------------------------------
source(file = "R/02A_Population_dynamics.R")
source(file ="R/99_functions.R")
#source(file = "R/11_sunrise_sunset_data.R")
#source(file = "R/10_ODE_rates.R")
source(file = "R/02C_Farm_info.R")

Farm_info <- Farm_var("C1")
First_sample <- Farm_info[[1]]
nCows <- Farm_info[[4]]

# Time values

month5 <- 152 
month10 <- 304
year <- 365


# Parameters --------------------------------------------------------------
time <- 2*365
First_DOB <- First_sample - 4*year
date <- First_sample
nE0 <- 1
ID_no <- nCows

sla_prop <- c(0.1,0.3,0.5)

#Placeholder to fill out
Pop <- matrix(NA, ncol = time, nrow = 3)
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


Pop[,1] <- nCows



for(i in 1:3){
 date <- First_sample
 sla_prop <- sla_prop[i]
 
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
  
  for(k in 2:time){
    
    
    date <- date + 1
  
    Farm <- cow_dynamics(Farm,sla_prop)
    
    
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
    
    print(time-k)  
    Pop[i,k] <- Farm %>% nrow()
  }
  
  
}

ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = Pop[1,],
                col = "0.1")) +
  geom_line(aes(y = Pop[2,],
                col = "0.3")) +
  geom_line(aes(y = Pop[3,],
                col = "0.5")) 
  


ggplot(mapping = aes(x = 1:time,
                     y = Births)) +
  geom_line()







