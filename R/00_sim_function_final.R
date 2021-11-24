rm(list = ls())
load("data/10_model_weather.RData")

sim.seed <- c(563514,150992,5680773,2372580,1941657,
              4065060,5462255,2833794,9486614,8365400)

run_simulation <- function(FarmID,nruns){

  # Import functions --------------------------------------------------------
  source(file ="R/99_functions.R")
  source(file = "R/02A_Population_dynamics.R")
  source(file = "R/02B_ODE_rates.R")
  source(file = "R/02C_Farm_info.R")
  library(tidyverse)
  library(lubridate)
  
  # Farm information
  Farm_info <- Farm_var(FarmID)
  First_sample <- Farm_info[[1]]
  City <- Farm_info[[2]]
  nCohort <- Farm_info[[3]]
  nCows <- Farm_info[[4]]
  nE0 <- floor(Farm_info[[7]]*nCows/nCohort) 
  
  
  # Time values --------------------------------------------------------------
  month5 <- 152 
  month10 <- 304
  year <- 365
  
  
  # Parameters --------------------------------------------------------------
  time <- as.integer(as.Date("2017-12-31")-First_sample)
  First_DOB <- First_sample - 4*year
  ID_no <- nCows
  M_scaling <- 10^-7
  egg_mu_scaled <- 2.773*(12000/5)
  egg_theta_distr <- 0.287
  
  
  # List to save results -----------------------------------------------
  sim_validation <- list()
  sim_IBM <- list()
  sim_ODE <- list()
  result <- list()
  
  
  # ODE_rate_function -------------------------------------------------------
  
  Rates <- function(date){
    
    # Defining the maximum rates 
    lambda_ES_max <- 0.0000005 #Transmission rate egg to snail
    mu_Egg_max <- 0.65 # Death rate eggs (become non-infectious)
    delta_snail_max <- 1.5 #Daily snail population "scaling factor"
    gamma_S_max <- 2 #Excretion of metacercarria from snail
    mu_S_max <- 0.1 #Death rate of infected snails / recovery rate
    mu_M_max <- 0.15 # Death rate of metacercaria
    
    
    #Filtering data for given day  
    DD <- daily_weather %>% filter(Date == date,location == City) %>% 
      select(mean_ground_temp_ten) %>% 
      pull()
    
    last10Days <- date - 0:9
    
    rain <- daily_weather %>% filter(Date %in% last10Days,location == City) %>% 
      summarise(rain = sum(rain)) %>% pull()
    
    rate_exp <-0.24
    
    if(rain < 2){
      
      lambda_ES <- 0
      mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max*1.5
      mu_S <- (1-pexp(q = DD,rate = rate_exp))*mu_S_max*1.5
      mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max*1.5
    }
    
    else {
      
      lambda_ES <- pexp(q = DD,rate = rate_exp)*lambda_ES_max
      mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max
      mu_S <- (1-pexp(q = DD,rate = rate_exp))*mu_S_max
      mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max
    }
    
    delta_snail <- pexp(q = DD,rate = rate_exp)*delta_snail_max
    Rates <- c(lambda_ES,mu_Egg,delta_snail,mu_M, mu_S)
    return(Rates) 
  }
  
  # Cow popualtion dynamics function
  cow_pop_init <- function(tibble){
    tibble <- tibble %>% 
      rowwise() %>% 
      mutate(State = if_else(CowID %in% E0_cows,
                             3-rbinom(1,1,0.5),
                             1),
             E_period = case_when(State == 2 ~ conv_neg(round(rnorm(1, mean = 7*7, sd = 7))),
                                  TRUE ~ 0),
             sick_period = case_when(State == 2 ~ sick_period + 1,
                                     State == 3 ~ conv_neg(round(rnorm(1, mean = 7*7, sd = 7))),
                                     TRUE ~ 0),
             sick_period = if_else(sick_period >= Age,
                                   Age,
                                   sick_period),
             I_period = case_when(State == 3 ~ 1,
                                  TRUE ~ 0),
             Lactation = case_when(Age >= 3*year ~ as.numeric(rbinom(1,1,5/6)),
                                   TRUE ~ 0),
             cycle_day = case_when(Lactation == 1 ~ round(runif(1,1,month10)),
                                   Lactation == 0 & Age >= 3*year ~ 
                                     round(runif(1,month10+1,year)),
                                   Group ==  3 & Age < 3*year ~ Age-2*year),
             Lactation = case_when(Group ==  3 & Age < 3*year & cycle_day >= 1 & cycle_day <= month10 ~ 
                                     1,
                                   Group ==  3 & Age < 3*year & cycle_day > month10 ~ 0,
                                   TRUE ~ Lactation),
             n_calf = case_when(Group == 3 & Age < 3*year ~ 1,
                                Group == 3 & (Age >= 3*year & Age <= 4*year) ~ 2,
                                TRUE ~ 0),
             Grazing = case_when(Lactation == 1 ~ runif(1,0.1,0.6),#Milking cows 
                                 #calf under 5 months do note graze
                                 Age <= month5 ~ runif(1,0,0.1), 
                                 #calf 5-9 month graze with heifers (2 years of age) 
                                 (Group == 1 & Age > month5) | Group == 2 ~ runif(1,0.5,1), 
                                 #Dry cows between lactation
                                 TRUE ~ runif(1,0.2,0.8)))
    return(tibble)
  }
  
  # Function to move cows through groups, lactation days and grazing 
  cow_dynamics <- function(tibble, sla_prob){
    tibble <- tibble %>% mutate(Age = date - DOB,
                                Group = case_when(Age > month10 & Age < 2*year ~ 2,
                                                  Age == 2*year ~ 3,
                                                  TRUE ~ as.numeric(Group)),
                                n_calf = case_when(Age == 2*year ~ 1,
                                                   cycle_day == year ~ n_calf + 1,
                                                   TRUE ~ n_calf),
                                cycle_day = case_when(cycle_day == year ~ 1,
                                                      Group == 3 & is.na(cycle_day) ~ 1,
                                                      cycle_day > 0 ~ cycle_day + 1,
                                                      TRUE ~ cycle_day),
                                Lactation = case_when(cycle_day >= 1 & cycle_day <= month10 ~ 1,
                                                      cycle_day > month10 ~ 0),
                                Grazing = case_when(Lactation == 1 ~ runif(1,0.1,0.6),#Milking cows 
                                                    #calf under 5 months do not grass
                                                    Age <= month5 ~ runif(1,0,0.1), 
                                                    #calf 5-9 month graze with heifers (2 years of age) 
                                                    (Group == 1 & Age > month5) | Group == 2 ~ runif(1,0.5,1), 
                                                    #Dry cows between lactation
                                                    TRUE ~ runif(1,0.2,0.8)),
                                Grazing = if_else(month(date) >= 4 & month(date) < 11,
                                                  Grazing*1,
                                                  Grazing*0.05))
    #Slaughter
    tibble <- tibble %>% mutate(slaughter = 
                                  case_when(cycle_day == month10 ~ as.numeric(rbinom(1,1,sla_prob)),
                                            TRUE ~ 0))
    tibble <- tibble %>% 
      filter(!(slaughter == 1))
    
    return(tibble)
  }
  
  
  
  
  
  for(sim_n in c(1:nruns)){

    set.seed(sim.seed[sim_n])
    # Reset date 
    date <- First_sample
    
    #Placeholder to fill out
    #Vectors to fill our in ODE
    Eggs <- c(rep(0,time))
    E1_S <- c(rep(0,time))  
    E2_S <- c(rep(0,time))
    I_S <-  c(rep(0,time))
    S_S <- c(rep(0,time))
    M <- c(rep(0,time))
    Snail_pop <- c(rep(0,time))
    Snail_prev <- c(rep(0,time))
    
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
    
    validation <- tibble(CowID = 0, 
                         DOB = NA,
                         I_period = 0,
                         Visit_day_no = 0)
    
    
    
    Egg_new <- c(rep(0,time))
    Births <- c(rep(0,time))
    Pop <- c(rep(0,time))
    
    
    
    # ODE Parameters --------------------------------------------------------------
    mu_Egg <- Rates(date)[2]
    lambda_ES <- Rates(date)[1]
    mu_M <- Rates(date)[4]
    Snail_pop0 <- 10^4
    alpha <- 2/(6*7)
    gamma_S <- 2
    mu_S <- Rates(date)[5]
    sla_prob <- 0.5
    
    
    Eggs[1] <- 0
    E1_S[1] <- 0
    E2_S[1] <- 0
    I_S[1] <- 0
    M[1] <- 100
    Snail_pop[1] <- Rates(date)[3]*Snail_pop0
    S_S[1] <- Snail_pop[1] - (E1_S[1]+E2_S[1]+I_S[1])
    
    
    # Creating data frame of susceptible cows 
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
    .GlobalEnv$E0_cows <- sample(1:nCows,
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
    
    
    for(k in 2:time){
      
      date <- date + 1
      
      # Moving through the different groups (cow population dynamics)
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
      mu_S <- Rates(date)[5]
      
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
      
    }
    
    sim_validation[[sim_n]] <- validation %>% slice(-1)
    sim_IBM[[sim_n]] <- list(S_Cow,E_Cow,I_Cow,Egg_new)
    sim_ODE[[sim_n]] <- list(Eggs, E1_S, E2_S, I_S, S_S, Snail_pop,M)
    
    print(sim_n)
    
  }
  
  result <- list(sim_validation, sim_IBM, sim_ODE)
  
  return(result)
  
}


result <- run_simulation("O1",10)

sim_validation <- result[[1]]
sim_ODE <- result[[3]]
sim_IBM <- result[[2]]



save(sim_validation,file = "results/validation_O1_01.RData")
save(sim_IBM,file = "results/IBM_O1_01.RData")
save(sim_ODE,file = "results/ODE_O1_01.RData")

