# Function to initiate the cow population
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
                               #Calfs under 5 months do note graze
                               Age <= month5 ~ runif(1,0,0.1), 
                               #Calfs 5-9 month graze with heifers (2 years of age) 
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
                                      #Calfs under 5 months do not grass
                                      Age <= month5 ~ runif(1,0,0.1), 
                                      #Calfs 5-9 month graze with heifers (2 years of age) 
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

  