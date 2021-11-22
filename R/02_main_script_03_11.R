# 02 main script

rm(list = ls())

#set.seed(1234)
#set.seed(756)
set.seed(744)


library(tidyverse)
library(lubridate)
library(statmod)
library(RColorBrewer)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")
source(file = "R/02A_Population_dynamics.R")
source(file = "R/02B_ODE_rates.R")
source(file = "R/02C_Farm_info.R")
load("data/10_model_weather.RData")

#Colors
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

# Initiation
Farm_info <- Farm_var("C1")

First_sample <- Farm_info[[1]]
City <- Farm_info[[2]]
nCohort <- Farm_info[[3]]
nCows <- Farm_info[[4]]
nE0 <- floor(Farm_info[[7]]*nCows/nCohort) 
# egg_mu_distr <- Farm_info[[8]]
# egg_theta_distr <- 0.4633
egg_mu_scaled <- 2.773*(12000/5)
egg_theta_distr <- 0.287


# Time values

month5 <- 152 
month10 <- 304
year <- 365


# Parameters --------------------------------------------------------------
time <- as.numeric(as.Date("2017-12-31")-First_sample)
First_DOB <- First_sample - 4*year
date <- First_sample
ID_no <- nCows
M_scaling <- 10^-7.5

#Placeholder to fill out
source(file = "R/02D_placeholders.R")

# ODE Parameters --------------------------------------------------------------
mu_Egg <- Rates(date)[2]
lambda_ES <- Rates(date)[1]
mu_M <- Rates(date)[4]
Snail_pop0 <- 10^4
alpha <- 2/(6*7)
gamma_S <- 2
mu_S <- 0.05
sla_prob <- 0.5


Eggs[1] <- 0
E1_S[1] <- 0
E2_S[1] <- 0
I_S[1] <- 0
M[1] <- 100
Snail_pop[1] <- Rates(date)[3]*Snail_pop0
S_S[1] <- Snail_pop[1] - (E1_S[1]+E2_S[1]+I_S[1])


#Vectors for exploration of model
DD_Temp <- c(rep(0,time))
DD_Temp[1] <- daily_weather %>% filter(Date == First_sample,location == City) %>% 
  select(mean_ground_temp_ten) %>% 
  pull()


#Rain
DD_Rain <- c(rep(0,time))
last10Days <- First_sample - 0:9
DD_Rain[1] <- daily_weather %>% filter(Date %in% last10Days,location == City) %>% 
  summarise(rain = sum(rain)) %>% pull()

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
               n_calf = 0,
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

# Random choose cohort
Cohort_cows <- cohort(Farm,Farm_info[[6]],nCohort)

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

Visit_days <- Farm_info[[5]]

starttime <- Sys.time()

for(k in 2:time){
  
  # # The E_prob is something that should be calculated and therefore it is just the grazing
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
  
  # Moving through the different groups (cow population dynamics)
  # 04x_cow_dynamics.R
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
  
  
  Farm <- Farm %>%  
    #CHANGE E_prob WITH E_PROB
    mutate(E_prob = 1-exp(-Grazing*M[k-1]*M_scaling),
           Exposed = case_when(State == 1 ~ rbinom(1,1,E_prob)),
           State = case_when(State == 2 & E_period == 0 ~ 3,
                             Exposed == 1 & State == 1 ~ 2,
                             TRUE ~ State),
           E_period = case_when(State == 2 & E_period == 0 ~ conv_neg(round(rnorm(1,mean = 5*7,sd = 7))),
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
  
  
  if(k %in% Visit_days){
    
    Cohort_info <- Farm %>% filter(CowID %in% Cohort_cows) %>% select(CowID, DOB, I_period, Group) %>% 
      mutate(Visit_day_no = k)
    validation <- bind_rows(validation, Cohort_info)
    
  }

  
  Farm <- Farm %>% mutate(eggs_pr_cow = case_when(sick_period > 2*30 & sick_period <= 3*30 
                                                   ~ rnbinom(1,egg_theta_distr,mu = egg_mu_scaled)*((1/(90-60))*sick_period-2),# Linear increasing from 0 to 1
                                                   sick_period > 3*30 & sick_period <= 8*30 
                                                   ~ rnbinom(1,egg_theta_distr,mu = egg_mu_scaled),
                                                   sick_period > 8*30 
                                                   ~ rnbinom(1,egg_theta_distr,mu = egg_mu_scaled)*exp(-(0.05*(sick_period-(8*30)))),
                                                   TRUE ~ 0))
  
  Egg_new[k] <- Farm %>% ungroup() %>% 
    summarise(sum(eggs_pr_cow)) %>% pull()
  
  # Add in mutate cow type and therefore how much faeces.   
  
  #Calcuting rates
  mu_Egg <- Rates(date)[2]
  lambda_ES <- Rates(date)[1]
  delta_snail <- Rates(date)[3]
  mu_M <- Rates(date)[4]
  
  Snail_pop[k] <- delta_snail*Snail_pop0
  S_S[k] <- Snail_pop[k] - (E1_S[k-1]+E2_S[k-1]+I_S[k-1])
  
  if(S_S[k] < 0){
    S_S[k] = 0 
  }
  
  Eggs[k] <- Eggs[k-1] + Egg_new[k]+(-mu_Egg * Eggs[k-1] - lambda_ES * Eggs[k-1] * S_S[k])
  E1_S[k] <- E1_S[k-1] + (lambda_ES * Eggs[k-1] * S_S[k] - alpha * E1_S[k-1])
  E2_S[k] <- E2_S[k-1] + (alpha * E1_S[k-1] - alpha * E2_S[k-1])
  I_S[k] <-  I_S[k-1] + (alpha * E2_S[k-1] - mu_S * I_S[k-1])
  M[k] <- M[k-1] + (gamma_S * I_S[k-1] - mu_M * M[k-1])
  
  
  if(Eggs[k] < 0){
    Eggs[k] = 0
  }
  
  if(Snail_pop[k] > 0){
    Snail_prev[k] = (E1_S[k-1]+E2_S[k-1]+I_S[k-1])/(E1_S[k-1]+E2_S[k-1]+I_S[k-1]+Snail_pop[k-1])
  } else {
    Snail_prev[k] = 0
  }
  
  
  Pop[k] <- Farm %>% nrow()
  print(time-k)  
  
  #Temperature for each day
  DD_Temp[k] <- daily_weather %>% filter(Date == date,location == City) %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
  # Rain
  last10Days <- date - 0:9
  DD_Rain[k] <- daily_weather %>% filter(Date %in% last10Days,location == City) %>% 
    summarise(rain = sum(rain)) %>% pull()
  
}


validation <- validation %>% slice(-1)

endtime <- Sys.time()                              

endtime - starttime                                                                         


Results <- list(S_Cow,E_Cow,I_Cow,Egg_new)



S_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = S1,
                col = "calf")) +
  geom_line(aes(y = S2,
                col = "Heifer")) +
  geom_line(aes(y = S3,
                col = "Primiparous")) +
  scale_color_brewer(palette = "Set2") +
  theme_bw()


E_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = E1,
                col = "calf")) +
  geom_line(aes(y = E2,
                col = "Heifer")) +
  geom_line(aes(y = E3,
                col = "Primiparous")) 


I_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = I1,
                col = "calf")) +
  geom_line(aes(y = I2,
                col = "Heifer")) +
  geom_line(aes(y = I3,
                col = "Primiparous"))

ggplot(mapping = aes(x = 1:time,
                     y = Pop)) +
  geom_line(col = color_scheme[1]) +
  #scale_color_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Time [days]",
       y = "Cattle [#]",
       title = "Population size") +
  theme(legend.position = "none")

ggplot(mapping = aes(x = 1:time,
                     y = Snail_pop)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = Egg_new)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = Eggs)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = M)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = E1_S)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = E2_S)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = I_S)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = S_S)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = Snail_prev)) +
  geom_line() 

ggplot(mapping = aes(x = 1:time,
                     y = DD_Temp)) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = DD_Rain)) +
  geom_line() + geom_hline(yintercept = 3, col = "red")





