# 04 IBM for life cycle stages in cows

rm(list = ls())

set.seed(1234)
#set.seed(756)

library(tidyverse)
library(lubridate)
library(statmod)


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
time <- year
First_sample <- as.Date("2015-04-27")
First_DOB <- First_sample - 3.75*year
date <- First_sample
ID_no <- nCows
M_scaling <- 10^7.5

#Placeholder to fill out
source(file = "R/98_placeholders.R")

#Collect Grazing over time
Graz_calf <- c(rep(0,time))
Graz_heifer <- c(rep(0,time))
Graz_group3 <- c(rep(0,time))

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

Graz_calf[1] <- Farm %>% filter(Group == 1) %>% 
  select(Grazing)

Graz_heifer[1] <- Farm %>% filter(Group == 2) %>% 
  select(Grazing)

Graz_group3[1] <- Farm %>% filter(Group == 3) %>% 
  select(Grazing)

Pop[1] <- nCows


#Graz_Calf <- matrix(0, ncol = time, nrow = nCows)

starttime <- Sys.time()

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
  #Slaughter
  Farm <- Farm %>% 
    filter(!(n_calfs >= 3 | date >= DOB + round(runif(1,3.75*year,6*year))))
  
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
  print(time-k)  
  
  #Temperature for each day
  DD_Temp[k] <- daily_weather %>% filter(Date == date,location == "Toender") %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
  
  #Grazing over time for each group
  Graz_calf[k] <- Farm %>% filter(Group == 1) %>% 
                            select(Grazing)
  
  Graz_heifer[k] <- Farm %>% filter(Group == 2) %>% 
    select(Grazing)
  
  Graz_group3[k] <- Farm %>% filter(Group == 3) %>% 
    select(Grazing)
  
  
  }

endtime <- Sys.time()                              

endtime - starttime                                                                         


Results <- list(S_Cow,E_Cow,I_Cow,Egg_new)

#Grazing index data prep
#Calfs
Graz_calfs <- tibble("day" = 1:time,
                    "index" = Graz_calf) 
Graz_calfs <- Graz_calfs  %>% unnest_longer("index")
mean_graz_calf <- Graz_calfs %>% group_by(day) %>% summarise(mean(index))
Graz_calf <- full_join(Graz_calf, mean_graz_calf, by = "x")

#Heifers
Graz_heifers <- tibble("day" = 1:time, 
                      "index" = Graz_heifer) 
Graz_heifers <- Graz_heifers  %>% unnest_longer("index")

#Group3
Graz_group3s <- tibble("day" = 1:time, 
                      "index" = Graz_group3) 
Graz_group3s <- Graz_group3s  %>% unnest_longer("index")



# Plots -------------------------------------------------------------------
#Calfs Grazing index over time
ggplot(data = Graz_calfs, aes(x = day,
                          y = index,
                          col = "calf")) +
  geom_point() + 
  stat_summary(
    geom = "line",
    fun = "mean",
    col = "blue") +
  labs(title = "Grazing index and mean over time for calf")
#Heifers Grazing index over time
Graz_heifers %>% ggplot(aes(x = day,
                           y = index,
                           col= "Heifer")) +
  geom_point()+ 
  stat_summary(
    geom = "line",
    fun = "mean",
    col = "black") +
  labs(title = "Grazing index and mean over time forfor heifers")

#Group3 Grazing index over time
Graz_group3s %>% ggplot(aes(x = day,
                           y = index,
                           col= "Group3")) +
  geom_point()  + 
  stat_summary(
    geom = "line",
    fun = "mean",
    col = "black")+
  labs(title = "Grazing index and mean over time for Group3")


#Base R Plots
# par(mar = c(2.5,2.2,1.3,0.5), family = "serif", font.lab = 2,mgp = c(1.4,0.4,0),mfrow=c(3,1))
# #Calfs Grazing index over time 
# plot(unlist(Graz_calf), ylab = "Calfs grazingindex", xlab = "", col=2)
# 
# #Heifers Grazing index over time
# plot(unlist(Graz_heifer), ylab = "Heifers grazingindex", xlab = "", col=3 )
# 
# #Group3 Grazing index over time
# plot(unlist(Graz_group3), ylab = "Group3 grazingindex", xlab = "", col=4)

