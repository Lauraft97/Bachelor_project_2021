# 07 Parameter calculations

rm(list = ls())

library(tidyverse)
library(patchwork)
library("RColorBrewer")
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")


# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")


# Convert to factors ------------------------------------------------------

fluke_data <- fluke_data %>% mutate(Group = factor(x = Group,
                                                   levels = c("Calf",
                                                              "Heifer",
                                                              "Primiparous",
                                                              "Multiparous")))

# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm
fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE))


Farm_name <- c("C1","C2","O1","O2")

Visit_name <- c("A","B","C","D","E","F","G")

n_Infected <- matrix(nrow = 4,
                     ncol = 7,
                     dimnames = list(Farm_name,Visit_name))

n_cow <- matrix(nrow = 4,
                ncol = 7,
                dimnames = list(Farm_name,Visit_name))

n_days <- matrix(nrow = 4,
                 ncol = 7,
                 dimnames = list(Farm_name,Visit_name))


for(k in 1:4){
  
Farm_data <- fluke_diag %>% filter(Farm == Farm_name[k])  

  for(i in 1:7){
  
  ID <- Farm_data %>% filter(Visit == Visit_name[i] & Diag == T) %>% 
                    select(UniqueID) %>% 
                    pull()
  
  n_Infected[k,i] <- length(ID)
  
  n_cow[k,i] <- Farm_data %>% filter(Visit == Visit_name[i]) %>% 
    distinct(UniqueID) %>% nrow()
  
  Farm_data <- Farm_data %>% filter(!(UniqueID %in% ID))
    
  }

dates <- append(as.Date("2014-08-01"), Farm_data %>% filter(Farm == Farm_name[k]) %>% 
                  arrange(Date) %>%  
                  distinct(Date) %>% pull() %>% as.Date()) 

n_days[k,] <- diff(dates)
  
}


# Infected per visit per cow susceptible in cohort
I_pr_cow <- n_Infected / n_cow

I_pr_cow_pr_day <- I_pr_cow / n_days

#Simulated probabilities
load("results/Meta_sim.RData")

M_scaling <- c(10^-8,10^-7,10^-6)
M_scaling_results <- as.matrix(M_scaling_results)

cow_prob_mean <- matrix(NA, nrow = 650, ncol = 3)

for(j in 1:3){
  M_scale <- M_scaling[j]
  Meta_pop <- M_scaling_results[,j]
  
  for(i in 1:650){
    Meta_pop_use <- Meta_pop[i]
    grazing <- runif(300,0,1)
    prob <- 1-exp(-grazing*Meta_pop_use*M_scale)
    
    cow_prob_mean[i,j] <- mean(prob)
  }
}

visits <- matrix(NA, nrow = 6, ncol = 3)

visits[1,] <- colMeans(cow_prob_mean[1:91,])
visits[2,] <- colMeans(cow_prob_mean[92:198,])
visits[3,] <- colMeans(cow_prob_mean[199:281,])
visits[4,] <- colMeans(cow_prob_mean[282:353,])
visits[5,] <- colMeans(cow_prob_mean[354:476,])
visits[6,] <- colMeans(cow_prob_mean[477:630,])

#Simulation and farm data in 1 tibble
sim_transmission <- tibble(Visit_name = c("B","C","D","E","F","G"),
                           simulation_M8 = visits[,1],
                           simulation_M7 = visits[,2],
                           simulation_M6 = visits[,3],
                           C1 = as.numeric(I_pr_cow_pr_day[1,-1]),
                           C2 = as.numeric(I_pr_cow_pr_day[2,-1]),
                           O1 = as.numeric(I_pr_cow_pr_day[3,-1]),
                           O2 = as.numeric(I_pr_cow_pr_day[4,-1]))

sim_transmission <- sim_transmission %>% 
  pivot_longer(!Visit_name, 
               names_to = "Farm_simulation", 
               values_to = "trans_prob")

save(sim_transmission,file = "results/sim_transmission.RData")

