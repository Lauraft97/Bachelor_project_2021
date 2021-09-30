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
time <- 365
Mcer <- 10^4

# Creating data frame of susceptible cows (average farm size in data 300)
Farm <- tibble(CowID = 1:nCows,
               Group = rep(1:4,75),
               State = 1,
               E_period = 0,
               I_period = 0,
               Grazing = runif(nCows,0.4,0.6))

Farm$Group <- factor(Farm$Group, levels=c(1:4))

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
         E_period = case_when(State == 2 ~ round(rnorm(1,mean = 6*7,sd = 7))),
         E_period = E_period[E_period < 0] <- 0 )


Farm <- Farm %>% 
  rowwise() %>% 
  mutate(State = if_else(CowID %in% E0_cows,
                         2,
                         1),
         E_period = case_when(State == 2 ~ round(rnorm(1,mean = 6*7,sd = 7))))

# E_period = E_period[E_period < 0] <- 0 


# If the distribution becomes negative then it will be chnaged to zero

# Examination of the normal distribution used
pnorm(0,42,7) # The likelihood of getting a negative value of very small. 

         
# Data frames to store results for each group

S_Cow <- tibble(S1 = rep(0,time),
                S2 = rep(0,time),
                S3 = rep(0,time),
                S4 = rep(0,time))

E_Cow <- tibble(E1 = rep(0,time),
                E2 = rep(0,time),
                E3 = rep(0,time),
                E4 = rep(0,time))

I_Cow <- tibble(I1 = rep(0,time),
                I2 = rep(0,time),
                I3 = rep(0,time),
                I4 = rep(0,time))

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


Eggs <- c(rep(0,time))


starttime <- Sys.time()

for(k in 2:time){
  
  # j <- 365 #Period = 365
  # tr <- k 
  # season <- 0.15*cos(2*pi*tr/j)+0.2
  # 
  # Mcer <- runif(1,10^4,11000)
  # 
  # # New grazing factor for each susceptible cow in each timestep
  # Farm <- Farm %>% mutate(Grazing = runif(1,0.4,0.6))
  # 
  # # # Identifying susceptibles and Exposed
  # # StateS <- Farm %>% filter(State == 1) %>% 
  # #   select(CowID) %>% 
  # #   pull()
  # # 
  # # StateE <- Farm %>% filter(State == 2) %>% 
  # #   select(CowID) %>% 
  # #   pull()
  # # 
  # # StateI <- Farm %>% filter(State == 3) %>% 
  # #   select(CowID) %>% 
  # #   pull()
  # 
  # 
  # # The E_prop is something that should be calculated and therefore it is just the grazing
  # #factor right now. 
  
  Farm <- Farm %>%  
    mutate(E_prop = Grazing*0.01,
           Exposed = case_when(State == 1 ~ rbinom(1,1,E_prop)),
           State = case_when(State == 2 & E_period == 0 ~ 3,
                             Exposed == 1 & State == 1 ~ 2,
                             TRUE ~ State),
           E_period = case_when(State == 2 & E_period == 0 ~ round(rnorm(1,mean = 6*7,sd = 7)),
                                State == 2 & E_period > 0 ~ E_period - 1,
                                E_period < 0 ~ 0,
                                TRUE ~ 0),
           I_period = case_when(State == 3 ~ I_period + 1))
  # 
  # Farm_1 <- Farm_1 %>%  
  #   mutate(E_prop = Grazing*0.01,
  #          Exposed = case_when(State == 1 ~ rbinom(1,1,E_prop)),
  #          State = case_when(State == 2 & E_period == 0 ~ 3,
  #                            Exposed == 1 & State == 1 ~ 2,
  #                            TRUE ~ State),
  #          E_period = case_when(State == 2 & E_period == 0 ~ round(rnorm(1,mean = 6*7,sd = 7)),
  #                               State == 2 & E_period > 0 ~ E_period - 1,
  #                               E_period < 0 ~ 0,
  #                               TRUE ~ 0),
  #          I_period = case_when(State == 3 ~ I_period + 1))
  
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
  
  Eggs[k] <- Eggs[k-1] + 
    
    
    
    
    x <- seq(0,100,0.001)
  
  inv_logit <- function(x) {
    return(1 / (1 + exp(- x)))
  }
  
  y <- inv_logit(x)
  
  plot(y ~ x)
    
    
    
    
# E_period = E_period[E_period < 0] <- 0
  
  
  # Determining what happens to susceptible cows
  # for(i in StateS){
  #   Grazing <- Farm %>% filter(CowID == i) %>% 
  #     select(Grazing) %>% 
  #     pull()
    
    #Likelihood of meeting/ingesting meta cercaria, probability
    #MC <- Grazing * Mcer * scaler
    
  
    
    #MC_prop <- pexp(MC,rate = 1)
    
    
    # If every cow has their own probability then every cow must draw their own
    #number (0,1) from the distribution
    
    #If vectorized (add calculation for the probability)
    # Farm <- Farm %>%  
    #   mutate(E_prop = Grazing,
    #          Exposed = case_when(State == 1 ~ rbinom(1,1,E_prop)))
    
    # if(MC_prop > 0.187){
    #   Farm$State[i] <- 2
    #   Farm$E_period[i] <- round(rnorm(1, mean = 5*7, sd = 2*7),0)
    
    


Eggs[k] <- Eggs[k-1] + Farm %>% filter(I_period > 0 & I_period < 10) %>% 
  nrow() * 3500



# S_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
#   filter(State == 1) %>% 
#   tally %>% 
#   pull() %>% 
#   t()
# 
# E_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
#   filter(State == 2) %>% 
#   tally %>% 
#   pull() %>% 
#   t()
# 
# 
# I_Cow[k,] <- Farm %>% group_by(Group, .drop = FALSE) %>%
#   filter(State == 3) %>% 
#   tally %>% 
#   pull() %>% 
#   t()

print(k)  

}

endtime <- Sys.time()                              

endtime - starttime                                                                         


Results <- list(S_Cow,E_Cow,I_Cow,Eggs)



S_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = S1,
                col = "calf")) +
  geom_line(aes(y = S2,
                col = "Heifer")) +
  geom_line(aes(y = S3,
                col = "Primiparous")) +
  geom_line(aes(y = S4,
                col = "Multiparous"))


E_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = E1,
                col = "calf")) +
  geom_line(aes(y = E2,
                col = "Heifer")) +
  geom_line(aes(y = E3,
                col = "Primiparous")) +
  geom_line(aes(y = E4,
                col = "Multiparous"))


I_Cow %>% ggplot(mapping = aes(x = 1:time)) +
  geom_line(aes(y = I1,
                col = "calf")) +
  geom_line(aes(y = I2,
                col = "Heifer")) +
  geom_line(aes(y = I3,
                col = "Primiparous")) +
  geom_line(aes(y = I4,
                col = "Multiparous"))





# T = 2*365
# j = 365 #Period = 4
# tr = 1:time
# y = 2*cos(2*pi*tr/j)
#plot(season, type="o", xlab="time", main=" A quarterly series with trend")
# abline(v = c(1, 4), col = "red", lty=2)


