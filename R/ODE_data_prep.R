rm(list=ls())

library(tidyverse)
library(patchwork)

source(file = "R/02C_Farm_info.R")


# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Egg_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))  
Egg_C1 <- Egg_C1 %>% mutate(date = First_sample + (1:time) - 1,
                            Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Egg_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))
Egg_C2 <- Egg_C2 %>% mutate(date = First_sample + (1:time) - 1,
                            Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Egg_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))  
Egg_O1 <- Egg_O1 %>% mutate(date = First_sample + (1:time) - 1,
                            Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Egg_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))  
Egg_O2 <- Egg_O2 %>% mutate(date = First_sample + (1:time) - 1,
                            Farm = "O2") 







# Load data -----------------------

# C2 is missing
Egg_total <- rbind(Egg_C1,Egg_C2,Egg_O1,Egg_O2) 

results_ODE_egg <- Egg_total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "Egg") %>% 
  select(Farm, date, variable, Q1, median, Q3)


# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/IBM_C1.Rdata")
nEgg_C1 <- as.data.frame(do.call(cbind, sapply(sim_IBM, "[", 4))) 
nEgg_C1 <- nEgg_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/IBM_C2.Rdata")
nEgg_C2 <- as.data.frame(do.call(cbind, sapply(sim_IBM, "[", 4)))
nEgg_C2 <- nEgg_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/IBM_O1.Rdata")
nEgg_O1 <- as.data.frame(do.call(cbind, sapply(sim_IBM, "[", 4))) 
nEgg_O1 <- nEgg_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/IBM_O2.Rdata")
nEgg_O2 <- as.data.frame(do.call(cbind, sapply(sim_IBM, "[", 4))) 
nEgg_O2 <- nEgg_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 


# C2 is missing
nEgg_total <- rbind(nEgg_C1,nEgg_C2,nEgg_O1,nEgg_O2) 

results_ODE_negg <- nEgg_total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "new_Egg") %>% 
  select(Farm, date, variable, Q1, median, Q3)



# Save eggs ---------------------------------------------------------------

Eggs <- bind_rows(results_ODE_egg,results_ODE_negg) %>% arrange(date)

save(Eggs, file = "results/Eggs.RData")
     
  
# Snails ------------------------------------------------------------------------


# Population --------------------------------------------------------------

## C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                            Farm = "C1") 
## C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

## O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

## O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1,Data_C2, Data_O1, Data_O2) 

results_ODE_Pop <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "pop") %>% 
  select(Farm, date, variable, Q1, median, Q3)


# Susceptible --------------------------------------------------------------

# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1, Data_C2, Data_O1, Data_O2) 

results_ODE_S <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "S") %>% 
  select(Farm, date, variable, Q1, median, Q3)



# Exposed_1 --------------------------------------------------------------

# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1, Data_C2, Data_O1, Data_O2) 

results_ODE_E1 <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "E1") %>% 
  select(Farm, date, variable, Q1, median, Q3)




# Exposed_2 --------------------------------------------------------------

# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1,Data_C2, Data_O1, Data_O2) 

results_ODE_E2 <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "E2") %>% 
  select(Farm, date, variable, Q1, median, Q3)


# Infected --------------------------------------------------------------

# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1,Data_C2, Data_O1, Data_O2) 

results_ODE_I <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "I") %>% 
  select(Farm, date, variable, Q1, median, Q3)



Snails <- bind_rows(results_ODE_Pop, results_ODE_S, results_ODE_E1, results_ODE_E2, results_ODE_I) %>% arrange(date)


save(Snails, file = "results/Snails.RData")



# Metacerceriae
# Exposed_2 --------------------------------------------------------------

# C1 ----------------------------------------------------------------
FarmID = "C1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C1.Rdata")
Data_C1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))  
Data_C1 <- Data_C1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C1") 
# C2 ----------------------------------------------------------------

FarmID = "C2"


time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_C2.Rdata")
Data_C2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))
Data_C2 <- Data_C2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "C2")

# O1 ----------------------------------------------------------------

FarmID = "O1"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O1.Rdata")
Data_O1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))  
Data_O1 <- Data_O1 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O1") 

# O2 ----------------------------------------------------------------

FarmID = "O2"

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
First_sample <- Farm_var(FarmID)[[1]]

load("results/ODE_O2.Rdata")
Data_O2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))  
Data_O2 <- Data_O2 %>% mutate(date = First_sample + (1:time) - 1,
                              Farm = "O2") 

total <- rbind(Data_C1,Data_C2, Data_O1, Data_O2) 

Metacercariae <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75)) %>% 
  select(Farm, date, variable, Q1, median, Q3)

save(Metacercariae, file = "results/Metacercariae.RData")

