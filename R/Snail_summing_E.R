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

total_E1 <- rbind(Data_C1, Data_C2, Data_O1, Data_O2) 




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

total_E2 <- rbind(Data_C1,Data_C2, Data_O1, Data_O2) 

E1 <- pivot_longer(data = total_E1, cols = starts_with("V"), names_to = "Simulation", values_to = "E1")
E2 <- pivot_longer(data = total_E2, cols = starts_with("V"), names_to = "Simulation", values_to = "E2")

total <- left_join(E1,E2) %>% mutate(E = E1 + E2) %>% 
         select(date,Farm,Simulation,E) %>% pivot_wider(names_from = Simulation, values_from = E)

results_ODE_E <- total %>%
  rowwise() %>% 
  mutate(median = median(c_across(starts_with("V"))),
         Q1 = quantile(c_across(starts_with("V")), 0.25),
         Q3 = quantile(c_across(starts_with("V")), 0.75),
         variable = "E") %>% 
  select(Farm, date, variable, Q1, median, Q3)

save(results_ODE_E, file = "results/ODE_E_total.RData")

