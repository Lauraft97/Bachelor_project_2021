rm(list = ls())

#set.seed(1234)
#set.seed(756)
set.seed(744)


library(tidyverse)
library(lubridate)
library(statmod)
library(extrafont)
loadfonts(quiet = T)

color_scheme <- RColorBrewer::brewer.pal(12, "Paired")[1:12]


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
time <- year*3
First_DOB <- First_sample - 4*year
nCohort <- Farm_info[[3]]
nCows <- Farm_info[[4]]
nE0 <- floor(Farm_info[[7]]*nCows/nCohort) 
ID_no <- nCows

sla_prob_vec <- seq(0.1,0.9,0.1)

#Placeholder to fill out
Pop <- matrix(NA, ncol = time, nrow = length(sla_prob_vec))
Births <- rep(0,time)

time.left <- time*length(sla_prob_vec)

for(i in 1:length(sla_prob_vec)){
 date <- First_sample
 sla_prob <- sla_prob_vec[i]
 
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
                n_calf = 0,
                cycle_day = 0,
                Grazing = 0,
                Age = as.numeric(First_sample-DOB))
 
 
 Farm$Group <- factor(Farm$Group, levels=c(1:3))
 
 E0_cows <- sample(1:nCows,
                   nE0,
                   replace = F)
 
 Pop[i,1] <- nCows
 Farm <- cow_pop_init(Farm)
  
  for(k in 2:time){
    date <- date + 1
  
    Farm <- cow_dynamics(Farm,sla_prob)
    
    
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
      new_calf <- tibble(CowID = (ID_no+1-Births[k]):ID_no,
                          DOB = date,
                          Group = 1,
                          Lactation = NA,
                          State = 1,
                          E_period = 0,
                          I_period = 0,
                          sick_period = 0,
                          n_calf = 0,
                          cycle_day = NA,
                          Grazing = runif(Births[k],0,0.1))
      
      Farm <- bind_rows(Farm,new_calf)
    }
    
    print(time.left)  
    Pop[i,k] <- Farm %>% nrow()
    time.left <- time.left - 1
  }
  
  
}


# Plotting ----------------------------------------------------------------

save(Pop,file = "results/Pop_0.1_0.9_full.RData")
#Loading data
#load("results/Pop_0.1_0.9.RData")
#load("results/Pop_0.4_0.6.RData")

# Spanding from 0.1 to 0.9
ggplot(mapping = aes(x = 1:time)) +
  scale_color_manual(values=c(color_scheme[9], 
                              color_scheme[2], 
                              color_scheme[5],
                              color_scheme[4],
                              color_scheme[6],
                              color_scheme[7],
                              color_scheme[8],
                              color_scheme[10],
                              color_scheme[12]),
                     name = "Slaughter \n probability") + 
  geom_line(aes(y = Pop[1,],
                col = "0.1")) +
  geom_line(aes(y = Pop[2,],
                col = "0.2")) +
  geom_line(aes(y = Pop[3,],
                col = "0.3")) +
  geom_line(aes(y = Pop[4,],
                col = "0.4")) +
  geom_line(aes(y = Pop[5,],
                col = "0.5")) +
  geom_line(aes(y = Pop[6,],
                col = "0.6")) +
  geom_line(aes(y = Pop[7,],
                col = "0.7")) +
  geom_line(aes(y = Pop[8,],
                col = "0.8")) +
  geom_line(aes(y = Pop[9,],
                col = "0.9")) +
  geom_hline(yintercept = 312,linetype = "dashed", col = "gray60", size = 0.5) +
  labs(title = "Cattle population",
       x = "Days",
       y = "Cattle [#]")+
  theme_bw(base_family = "Lucida Bright",
           base_size = 12)

ggsave(filename = "results/figures/stable_pop.png")
  







