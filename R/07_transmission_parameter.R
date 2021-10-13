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

I_pr_cow_pr_day[,-1]




