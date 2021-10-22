# 04 IBM for life cycle stages in cows

rm(list = ls())

library(tidyverse)
library(lubridate)
library(statmod)

#Gathering slaugher, births and population size
Pop_sim <- matrix(nrow = 10, ncol = 633)

Slaughter_sim <- matrix(nrow = 10, ncol = 633)

Births_sim <- matrix(nrow = 10, ncol = 633)

left <- 633*10

for(i in 1:10){


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")
source(file = "R/04x_cow_dynamics.R")
#source(file = "R/11_sunrise_sunset_data.R")
#source(file = "R/10_ODE_rates.R")
source(file = "R/10_ODE_temp_mean.R")
load("data/10_model_weather.RData")


# Model IBM ------------
# - Each cow is a part of one of the four group
# - Each cow has a grazing intensity which is an expression of the likelihood of grazing.
# - Calf are not on grass for the first 4-6 month of their life

# Time values

month5 <- 152 
month10 <- 304
year <- 365

# Parameters --------------------------------------------------------------
nCows <- 300
nE0 <- 3
time <- 633
First_sample <- as.Date("2015-04-27")
First_DOB <- First_sample - 3.75*year
date <- First_sample
ID_no <- nCows
M_scaling <- 10^7.5

#Placeholder to fill out
source(file = "R/98_placeholders.R")

# ODE Parameters --------------------------------------------------------------
mu_Egg <- Rates(date)[2]
lambda_ES <- Rates(date)[1]
mu_M <- Rates(date)[4]
Snail_pop0 <- 10^4
alpha <- 2/(6*7)
gamma_S <- 2
mu_S <- 0.05


Eggs[1] <- 10000
E1_S[1] <- 100
E2_S[1] <- 0
I_S[1] <- 0
R_S[1] <- 0
M[1] <- 100
Snail_pop[1] <- 0.5*Snail_pop0

#Vectors for exploration of model
DD_Temp <- c(rep(0,time))
DD_Temp[1] <- daily_weather %>% filter(Date == First_sample,location == "Toender") %>% 
  select(mean_ground_temp_ten) %>% 
  pull()

# Creating data frame of susceptible cows (average farm size in data 300)
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

# Random choose E0 cows
E0_cows <- sample(1:nCows,
                  nE0,
                  replace = F)


# Randomly exposed nE0 number of cows (change state to 2) and generating period to be in E
Farm <- cow_pop_init(Farm)

# Examination of the normal distribution used
#pnorm(0,49,7) # The likelihood of getting a negative value of very small. 

S_Cow[1,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
  filter(State == 1) %>% 
  tally %>% 
  pull() %>% 
  t()

E_Cow[1,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
  filter(State == 2) %>% 
  tally %>% 
  pull() %>% 
  t()

I_Cow[1,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
  filter(State == 3) %>% 
  tally %>% 
  pull() %>% 
  t()

Pop[1] <- nCows

#Slaughter and total births
Slaughter_pr_day <- rep(0,time)
Births_pr_day <- rep(0,time)

for(k in 2:time){
  
  # # The E_prop is something that should be calculated and therefore it is just the grazing
  # #factor right now. 
  
  #FARM tibble
  # slagte efter kalve og alder (justerer i forhold til konstant population)
  # 2 år føde en kalv og rykke gruppe (derved blive lactating (1) og cycle day = 0)
  # KALVE SÆT TIL 1 INITIELT FOR GRUPPE 3
  # Gruppe 3: Hvis cycle = 300 rykke fra lactating 1 til 0
  # Hvis cycle = 365 føde en kalv og lactating (1) og cycle day = 0
  
  # Summarizing births 
  # Tælle kalve der er blevet født i tidsskridtet og fjerne halvdelen (tyre kalve)
  # Tilføje det fødte antal køer i tibble i gruppe 1, dagens dato osv. 
  # ID tilføje if statement så ID kun bliver opdateret hvis der er blevet født kalve
  
  date <- date + 1
  
  #How many will be slaughtered?
  Slaughter_pr_day[k] <- Farm %>% 
    filter(n_calfs >= 3 | date >= DOB + round(runif(1,3.75*year,6*year))) %>% 
    nrow()
  
  #Slaughter
  Farm <- Farm %>% 
    filter(!(n_calfs >= 3 | date >= DOB + round(runif(1,3.75*year,6*year))))
  
  # Count the number of cows who will have a calf
  Births[k] <- Farm %>% filter(Age == 2*year | cycle_day == year) %>% nrow()
  
  #Total births
  Births_pr_day[k] <- Farm %>% filter(Age == 2*year | cycle_day == year) %>% nrow()
  
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
  
  
  Farm <- Farm %>%  
    mutate(E_prop = 1-exp(-Grazing*(M[k-1]/M_scaling)),
           Exposed = case_when(State == 1 ~ rbinom(1,1,E_prop)),
           State = case_when(State == 2 & E_period == 0 ~ 3,
                             Exposed == 1 & State == 1 ~ 2,
                             TRUE ~ State),
           E_period = case_when(State == 2 & E_period == 0 ~ conv_neg(round(rnorm(1,mean = 7*7,sd = 7))),
                                State == 2 & E_period > 0 ~ E_period - 1,
                                E_period < 0 ~ 0,
                                TRUE ~ 0),
           I_period = case_when(State == 3 ~ I_period + 1,
                                TRUE ~ 0),
           sick_period = case_when(State == 2 & E_period >= 0 ~ sick_period + 1,
                                   State == 3 ~ sick_period + 1,
                                   TRUE ~ 0))
  
  
  
  
  S_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
    filter(State == 1) %>% 
    tally %>% 
    pull() %>% 
    t()
  
  E_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
    filter(State == 2) %>% 
    tally %>% 
    pull() %>% 
    t()
  
  
  I_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
    filter(State == 3) %>% 
    tally %>% 
    pull() %>% 
    t()
  
  Farm <- Farm %>% mutate(eggs_pr_gram = case_when(sick_period > 2*30 & sick_period <= 3*30 
                                                   ~ log_growth(round(rinvgauss(1,mean = 1.52, shape = 0.35),2),75,0.2,sick_period),
                                                   sick_period > 3*30 & sick_period <= 8*30 
                                                   ~ round(rinvgauss(1,mean = 1.52, shape = 0.35),2),
                                                   sick_period > 8*30 
                                                   ~ round(rinvgauss(1,mean = 1.52, shape = 0.35),2)*exp(-(0.05*(sick_period-(8*30)))),
                                                   TRUE ~ 0))
  
  Egg_new[k] <- Farm %>% filter(eggs_pr_gram > 0) %>% 
    mutate(egg_excreted = eggs_pr_gram * 3000) %>%
    ungroup() %>% 
    summarise(sum(egg_excreted)) %>% pull()
  
  # Add in mutate cow type and therefore how much faeces.   
  
  #Calcuting rates
  mu_Egg <- Rates(date)[2]
  lambda_ES <- Rates(date)[1]
  delta_snail <- Rates(date)[3]
  mu_M <- Rates(date)[4]
  
  #Snail_pop[k] <- season_snail_pop(0.9,2*pi/year,-25,1,k)*Snail_pop0
  Snail_pop[k] <- delta_snail*Snail_pop0
  Eggs[k] <- Eggs[k-1] + Egg_new[k]+(-mu_Egg * Eggs[k-1] - lambda_ES * Eggs[k-1] * Snail_pop[k-1])
  E1_S[k] <- E1_S[k-1] + (lambda_ES * Eggs[k-1] * Snail_pop[k-1] - alpha * E1_S[k-1])
  E2_S[k] <- E2_S[k-1] + (alpha * E1_S[k-1] - alpha * E2_S[k-1])
  I_S[k] <-  I_S[k-1] + (alpha * E2_S[k-1] - mu_S * I_S[k-1])
  R_S[k] <-  R_S[k-1] + (mu_S * I_S[k-1])
  M[k] <- M[k-1] + (gamma_S * I_S[k-1] - mu_M * M[k-1])
  
  
  if(Eggs[k] < 0){
    Eggs[k] = 0
  }
  
  
  Pop[k] <- Farm %>% nrow()
  left <- left-1
  print(left)  
  
  #Temperature for each day
  DD_Temp[k] <- daily_weather %>% filter(Date == date,location == "Toender") %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
}

Results <- list(S_Cow,E_Cow,I_Cow,Egg_new)
#Gathering slaugher, births and population size
#Pop_sim <- matrix(nrow = 7, ncol = time)
Pop_sim[i,] <- Pop

#Slaughter_sim <- matrix(nrow = 7, ncol = time)
Slaughter_sim[i,] <- Slaughter_pr_day

#Births_sim <- matrix(nrow = 7, ncol = time)
Births_sim[i,] <- Births_pr_day

}
#Population over time - plot
# plot(x = 1:time, y = Pop_sim[1,], col = 1, type = "l")
# lines(x = 1:time, y = Pop_sim[2,], col = 2)
# lines(x = 1:time, y = Pop_sim[3,], col = 3)
# lines(x = 1:time, y = Pop_sim[4,], col = 4)
# lines(x = 1:time, y = Pop_sim[5,], col = 5)
# lines(x = 1:time, y = Pop_sim[6,], col = 6)
# lines(x = 1:time, y = Pop_sim[7,], col = 7)
# lines(x = 1:time, y = Pop_sim[8,], col = 8)
# lines(x = 1:time, y = Pop_sim[9,], col = 9)
# lines(x = 1:time, y = Pop_sim[10,], col = 10)
# abline(h = 300, lty = 2)
# 
save(Pop_sim,file = "data/Simulations_SSL/Pop_sim_1.RData")
save(Slaughter_sim,file = "data/Simulations_SSL/Slaughter_sim_1.RData")
save(Births_sim,file = "data/Simulations_SSL/Births_sim_1.RData")
# 
# plot(x = 1:time, y = Pop_sim[1,], col = 1, type = "l",
#      ylim = c(270,350))
# lines(x = 1:time, y = Pop, col = 2)








