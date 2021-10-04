# 04 IBM for life cycle stages in cows

rm(list = ls())

set.seed(1234)

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")


# Model IBM ------------
# - Each cow is a part of one of the four group
# - Each cow has a grazing intensity which is an expression of the likelihood of grazing.
# - Calf are not on grass for the first 4-6 month of their life


# Parameters --------------------------------------------------------------
nCows <- 300
nE0 <- 3
time <- 300
Mcer <- 10^4
First_sample <- as.Date("2015-04-27")
First_DOB <- First_sample - 4*365
date <- First_sample
ID_no <- nCows

# ODE Parameters --------------------------------------------------------------
mu_Egg <- 0.1
lambda_ES <- 2 * 10^(-10)
S_S <- 10^4
alpha <- 2/(6*7)
gamma_S <- 2
mu_S <- 0.05
mu_M <- 0.05

#Vectors to fill our in ODE
Eggs <- c(rep(0,time))
E1_S <- c(rep(0,time))  
E2_S <- c(rep(0,time))
I_S <-  c(rep(0,time))
R_S <-  c(rep(0,time))
M <- c(rep(0,time))

Eggs[1] <- 10
E1_S[1] <- 0
E2_S[1] <- 0
I_S[1] <- 0
R_S[1] <- 0
M[1] <- 1000
 
# Creating data frame of susceptible cows (average farm size in data 300)
Farm <- tibble(CowID = 1:nCows,
               DOB = as.Date(x = rdunif(n = nCows,
                                        a = as.integer(First_DOB),
                                        b = as.integer(First_sample)),
                             origin = "1970-01-01"),
               Group = case_when(First_sample - DOB <= 10*30 ~ 1,
                                 First_sample - DOB > 10*30 & First_sample - DOB <= 2*365 ~ 2,
                                 First_sample - DOB > 2*365 ~ 3),
               Lactation = 0,
               State = 1,
               E_period = 0,
               I_period = 0,
               sick_period = 0,
               n_calfs = 0,
               cycle_day = 0,
               Grazing = runif(nCows,0.4,0.6))


Farm$Group <- factor(Farm$Group, levels=c(1:3))

# Random choose E0 cows
E0_cows <- sample(1:nCows,
                  nE0,
                  replace = F)


# Randomly exposed nE0 number of cows (change state to 2) and generating period to be in E
Farm <- Farm %>% 
  rowwise() %>% 
  mutate(State = if_else(CowID %in% E0_cows,
                         2,
                         1),
         E_period = case_when(State == 2 ~ round(rnorm(1,mean = 7*7,sd = 7)),
                              E_period < 0 ~ 0,
                              TRUE ~ 0),
         sick_period = case_when(State == 2 ~ sick_period + 1,
                                 TRUE ~ 0),
         Lactation = case_when(Group == 3 ~ as.numeric(rbinom(1,1,5/6)),
                               TRUE ~ 0),
         cycle_day = case_when(Lactation == 1 ~ round(runif(1,1,305)),
                               Lactation == 0 & Group == 3 ~ round(runif(1,306,365))),
         n_calfs = case_when(Group == 3 & First_sample - DOB < 3*365 ~ 1,
                             Group == 3 & (First_sample - DOB >= 3*365 & First_sample - DOB <= 4*365) ~ 2,
                             TRUE ~ 0))

# If the distribution becomes negative then it will be changed to zero

# Examination of the normal distribution used
pnorm(0,49,7) # The likelihood of getting a negative value of very small. 

         
# Data frames to store results for each group

S_Cow <- tibble(S1 = rep(0,time),
                S2 = rep(0,time),
                S3 = rep(0,time))

E_Cow <- tibble(E1 = rep(0,time),
                E2 = rep(0,time),
                E3 = rep(0,time))

I_Cow <- tibble(I1 = rep(0,time),
                I2 = rep(0,time),
                I3 = rep(0,time))

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


Egg_new <- c(rep(0,time))
Births <- c(rep(0,time))
Pop <- c(rep(0,time))

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
    filter(!(n_calfs >= 3 | date >= DOB + round(runif(1,4.5*365,6*365))))
  
  # Count the number of cows who will have a calf
  Births[k] <- Farm %>% filter(date - DOB == 2*365 | cycle_day == 365) %>% nrow()
  
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
  Farm <- Farm %>% mutate(Group = case_when(date - DOB > 305 & date - DOB < 2*365 ~ 2,
                                      date - DOB == 2*365 ~ 3,
                                      TRUE ~ as.numeric(Group)),
                          n_calfs = case_when(date - DOB == 2*365 ~ 1,
                                              cycle_day == 365 ~ n_calfs + 1,
                                              TRUE ~ n_calfs),
                          cycle_day = case_when(cycle_day == 365 ~ 1,
                                                Group == 3 & is.na(cycle_day) ~ 1,
                                                cycle_day > 0 ~ cycle_day + 1,
                                                TRUE ~ cycle_day),
                          Lactation = case_when(cycle_day >= 1 & cycle_day <= 305 ~ 1,
                                                cycle_day > 305 ~ 0))
  
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
                   Grazing = runif(Births[k],0.4,0.6))
    
    Farm <- bind_rows(Farm,new_calfs)
  }
  
  Farm <- Farm %>%  
    mutate(E_prop = Grazing*0.01,
           Exposed = case_when(State == 1 ~ rbinom(1,1,E_prop)),
           State = case_when(State == 2 & E_period == 0 ~ 3,
                             Exposed == 1 & State == 1 ~ 2,
                             TRUE ~ State),
           E_period = case_when(State == 2 & E_period == 0 ~ round(rnorm(1,mean = 7*7,sd = 7)),
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
  
  Farm <- Farm %>% mutate(eggs_pr_5gram = case_when(sick_period > 2*30 & sick_period <= 3*30 
                                                ~ 0.5*runif(1,20,120),
                                                sick_period > 3*30 & sick_period <= 8*30 
                                                ~ runif(1,20,120),
                                                sick_period > 8*30 
                                                ~ runif(1,20,120)*exp(-(0.05*sick_period-(8*30))),
                                                TRUE ~ 0))
  
  n_cow_egg <- Farm %>% filter(eggs_pr_5gram > 0) %>% nrow()
# Add in mutate cow type and therefore how much faeces.   
  
Egg_new[k] <- round(sum(Farm$eggs_pr_5gram))*n_cow_egg*3000/5

Eggs[k] <- Eggs[k-1] + Egg_new[k]+(-mu_Egg * Eggs[k-1] - lambda_ES * Eggs[k-1] * S_S)
E1_S[k] <- E1_S[k-1] + (lambda_ES * Eggs[k-1] * S_S - alpha * E1_S[k-1])
E2_S[k] <- E2_S[k-1] + (alpha * E1_S[k-1] - alpha * E2_S[k-1])
I_S[k] <-  I_S[k-1] + (alpha * E2_S[k-1] - mu_S * I_S[k-1])
R_S[k] <-  R_S[k-1] + (mu_S * I_S[k-1])
M[k] <- M[k-1] + (gamma_S * I_S[k-1] - mu_M * M[k-1])

  #   x <- seq(0,100,0.001)
  # 
  # inv_logit <- function(x) {
  #   return(1 / (1 + exp(- x)))
  # }
  # 
  # y <- inv_logit(x)
  # 
  # plot(y ~ x)
  # 
  #   
    

Pop[k] <- Farm %>% nrow()
print(k)  

}

endtime <- Sys.time()                              

endtime - starttime                                                                         


Results <- list(S_Cow,E_Cow,I_Cow,Egg_new)



S_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = S1,
                col = "calf")) +
  geom_line(aes(y = S2,
                col = "Heifer")) +
  geom_line(aes(y = S3,
                col = "Primiparous")) 


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

ggplot(mapping = aes(x=1:200,y = Eggs[1:200])) +
  geom_line()

ggplot(mapping = aes(x = 1:time,
                     y = Egg_new)) +
  geom_line()



# T = 2*365
# j = 365 #Period = 4
# tr = 1:time
# y = 2*cos(2*pi*tr/j)
#plot(season, type="o", xlab="time", main=" A quarterly series with trend")
# abline(v = c(1, 4), col = "red", lty=2)


