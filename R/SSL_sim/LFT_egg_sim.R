# 04 IBM for life cycle stages in cows

rm(list = ls())

library(tidyverse)
library(lubridate)
library(statmod)

#Gathering slaugher, births and population size
t <- 633
Eggs_new_sim <- matrix(nrow = 5, ncol = t)
Eggs_one_cow <- matrix(nrow = 5, ncol = t)


left <- t*5

for(i in 1:5){
  
  
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
  time <- t
  First_sample <- as.Date("2015-04-27")
  First_DOB <- First_sample - 3.75*year
  date <- First_sample
  ID_no <- nCows
  M_scaling <- 10^7.5
  
  #Placeholder to fill out
  source(file = "R/98_placeholders.R")
  Eggs_over_time <- tibble(sick_period = 0,
                           eggs_pr_gram = 0)
  Egg_CC <- rep(0,t)
  
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
  
  CC <- Farm %>% filter(Group == 1) %>% select(CowID) %>% pull()
  CC_ind <- sample(1:length(CC),
                   1,
                   replace = F)
  CC <- CC[CC_ind]
  
  
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
                                                     ~ round(rinvgauss(1,mean = 1.52, shape = 0.3),2)*((1/(90-60))*sick_period-2),# Linear increasing from 0 to 1
                                                     sick_period > 3*30 & sick_period <= 8*30 
                                                     ~ round(rinvgauss(1,mean = 1.52, shape = 0.3),2),
                                                     sick_period > 8*30 
                                                     ~ round(rinvgauss(1,mean = 1.52, shape = 0.3),2)*exp(-(0.05*(sick_period-(8*30)))),
                                                     TRUE ~ 0))
    
    Sick_days_egg <- Farm %>% filter(eggs_pr_gram > 0) %>% select(sick_period,eggs_pr_gram)
    
    Eggs_over_time <- bind_rows(Eggs_over_time,Sick_days_egg)
    
    
    Egg_CC[k] <- Farm %>% filter(CowID == CC) %>% select(eggs_pr_gram) %>% pull()
    
    
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
  
  #Gathering new eggs in the different simulations
  Eggs_new_sim[i,] <- Egg_new
  Eggs_one_cow[i,] <- Egg_CC
  

}

#New egss excreted in total for the herd in different timesteps

mean_sim1 <- rep(0,t)
sd_sim1 <- rep(0,t)

for (j in 1:t){
  mean_sim1[j] <- mean(Eggs_new_sim[,j])
  
  sd_sim1[j] <- sd(Eggs_new_sim[,j])
}


ggplot(mapping = aes(x = 1:t)) +
  geom_line(aes(y = mean_sim1), colour = "red") +
  geom_line(aes(y = mean_sim1+sd_sim1),
            linetype = 2) +
  geom_line(aes(y = mean_sim1-sd_sim1),
            linetype = 2) +
  labs(title = "Simulation",
       subtitle = "New eggs excreted in total")



# Eggs pr gram excreted per cow in time
Eggs_over_time %>% filter(sick_period > 0) %>%  ggplot(aes(x = sick_period,
                              y = eggs_pr_gram)) +
  geom_point()

Eggs_over_time %>% group_by(sick_period) %>% summarise(mean_egg = mean(eggs_pr_gram),
                                                       median_egg = median(eggs_pr_gram),
                                                       quantile_egg = quantile(eggs_pr_gram))


number_summary <- Eggs_over_time %>% 
  group_by(sick_period) %>% 
  summarise_at(vars(eggs_pr_gram),
               list(min = min, Q1 = ~ quantile(., probs = 0.25),
                    median = median, Q3 = ~ quantile(., probs = 0.75),
                    max = max,
                    mean = mean)) %>% slice(-1)

number_summary %>% ggplot(aes(x = sick_period)) +
  geom_line(aes(y = Q1,
                colour = "Q1")) +
  geom_line(aes(y = median,
                col = "Median")) + 
  geom_line(aes(y = Q3,
                col = "Q3")) +
  geom_vline(aes(xintercept = 90),linetype = "dashed" ) +
  geom_vline(aes(xintercept = 240),linetype = "dashed" ) +
  labs(title = "Simulation",
       subtitle = "Eggs pr gram against sick_period") +
  xlab("Days after infection") +
  ylab("Eggs pr gram") +
  xlim(60,450) 

# Boxplot
Eggs_over_time %>% ggplot(aes(x = eggs_pr_gram)) + geom_boxplot() +
  coord_flip()

Eggs_over_time %>% ggplot(aes(x = eggs_pr_gram)) +
                   geom_histogram()

Eggs_over_time %>% ggplot(aes(x = eggs_pr_gram)) +
  geom_density(fill = "blue", alpha = 0.25)

Eggs_over_time %>% arrange(desc(eggs_pr_gram))


# Time profile for a single cow in the simulations

ggplot(mapping = aes(x = 1:t)) +
  geom_line(aes(
    y = Eggs_one_cow[2,])) +
  xlim(0,t)

# Examining one cow
cow_2 <- Eggs_one_cow[2,]
cow_2 <- cow_2[cow_2 > 0]

cow_2 <- tibble(sick_period = c(61:(length(cow_2)+60)), eggs = cow_2)

cow_2 %>% ggplot(aes(x = sick_period,
                     y = eggs)) +
  geom_line()


save(Eggs_over_time,file = "data/Simulations_SSL/Eggs_sim1.RData")
save(Eggs_new_sim,file = "data/Simulations_SSL/Eggs_new_sim1.RData")
save(Eggs_one_cow,file = "data/Simulations_SSL/Eggs_one_cow_sim1.RData")



