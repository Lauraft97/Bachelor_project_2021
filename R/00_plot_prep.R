rm(list=ls())

source(file = "R/02C_Farm_info.R")

library(tidyverse)

FarmID = "C1"
nruns = 3


load(paste0("results/validation_",FarmID,".Rdata"))
load(paste0("results/IBM_",FarmID,".Rdata"))
load(paste0("results/ODE_",FarmID,".Rdata"))

time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])


# Validation data
validation <- bind_rows(sim_validation, .id = "Simulation")

#IBM data

IBM_S <- bind_rows(sapply(sim_IBM, "[", 1))
IBM_E <- bind_rows(sapply(sim_IBM, "[", 2))
IBM_I <- bind_rows(sapply(sim_IBM, "[", 3))
IBM_new_Egg <- as.data.frame(do.call(cbind, sapply(sim_IBM, "[", 4)))   


#ODE data
ODE_Egg <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))   
ODE_E1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))   
ODE_E2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))   
ODE_I <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))
ODE_SS <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))
ODE_snail_pop <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))
ODE_M <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))   

# Mean and conf interval

results_IBM_S <- IBM_S %>% 
  mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
  pivot_longer(cols = starts_with("S"), names_to = "Group", values_to = "Count") %>%
  group_by(timestep, Group) %>%
  summarise(mean = mean(Count, na.rm = TRUE),
            sd = sd(Count, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

results_IBM_E <- IBM_E %>% 
  mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
  pivot_longer(cols = starts_with("E"), names_to = "Group", values_to = "Count") %>%
  group_by(timestep, Group) %>%
  summarise(mean = mean(Count, na.rm = TRUE),
            sd = sd(Count, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

results_IBM_I <- IBM_I %>% 
  mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
  pivot_longer(cols = starts_with("I"), names_to = "Group", values_to = "Count") %>%
  group_by(timestep, Group) %>%
  summarise(mean = mean(Count, na.rm = TRUE),
            sd = sd(Count, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)


results_ODE_Egg <- ODE_Egg %>%
  rowwise() %>% 
  mutate(mean = mean(c(V1,V2,V3)),
            sd = sd(c(V1,V2,V3)),
            n = length(c(V1,V2,V3)),
         se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
         lower.ci = if_else(lower.ci < 0,0,lower.ci)) 





# Plot example 
results_IBM_E %>% ggplot(aes(x = timestep,
                             y = mean,
                             color = Group)) +
  geom_line() +
  geom_line(aes(y = lower.ci,
                color = Group),
            linetype = 2) +
  geom_line(aes(y = upper.ci,
                color = Group),
            linetype = 2) +
  facet_wrap(~ Group)


results_ODE_Egg %>% ggplot(aes(x = 1:time,
                             y = mean)) +
  geom_line() +
  geom_line(aes(y = lower.ci,
                color = "Lower"),
            linetype = 2) +
  geom_line(aes(y = upper.ci,
                color = "Lower"),
            linetype = 2)

















