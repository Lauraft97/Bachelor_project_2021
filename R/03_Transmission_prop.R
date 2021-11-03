# 07 Parameter calculations

rm(list = ls())

library(tidyverse)


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

a <- fluke_data %>% filter(dEPG == TRUE)

a %>% group_by(Farm) %>% distinct(UniqueID) %>% summarise(n = n())
fluke_diag %>% filter(Diag == TRUE) %>% group_by(Farm) %>% distinct(UniqueID) %>% summarise(n = n())

fluke_data %>% group_by(Farm) %>% distinct(UniqueID) %>%  summarise(n = n())

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
  
  n_cow[k,i] <- Farm_data %>% distinct(UniqueID) %>% 
    nrow()
  
  Farm_data <- Farm_data %>% filter(!(UniqueID %in% ID))
    
  }

dates <- append(as.Date("2014-08-01"), Farm_data %>% filter(Farm == Farm_name[k]) %>% 
                  arrange(Date) %>%  
                  distinct(Date) %>% pull() %>% as.Date()) 

n_days[k,] <- diff(dates)
  
}


# Infected per visit per cow susceptible in cohort
I_pr_cow <- n_Infected / n_cow

I_pr_cow[,-1]

I_pr_cow_pr_day <- I_pr_cow / n_days

I_pr_cow_pr_day[,-1]*300



#Probabilities for infection

metac <- M

x <- 10^4

cow_prop_mean <- seq(1,length(metac))

for(i in 1:length(metac)){
  
  M_1 <- metac[i]
  grazing <- runif(300,0,1)
  prop <- 1-exp(-grazing*M_1/x)
  
  cow_prop_mean[i] <- mean(prop)
  
}


visits <- seq(1,6)

visits[1] <- mean(cow_prop_mean[1:95])
visits[2] <- mean(cow_prop_mean[96:195])
visits[3] <- mean(cow_prop_mean[196:276])
visits[4] <- mean(cow_prop_mean[277:345])
visits[5] <- mean(cow_prop_mean[346:474])
visits[6] <- mean(cow_prop_mean[475:620])


O1 <- I_pr_cow_pr_day[3,-1]*300
C1 <- I_pr_cow_pr_day[1,-1]*300
O2 <- I_pr_cow_pr_day[4,-1]*300
C2 <- I_pr_cow_pr_day[2,-1]*300



plot(cow_prop_mean) 

plot(visits)
points(O1,col = 2)
points(C1,col = 3)
points(O2,col = 4)
points(C2,col = 5)
abline(h=O1,col = "red")
abline(h=C1,col = "green")
abline(h=O2,col = "blue")





