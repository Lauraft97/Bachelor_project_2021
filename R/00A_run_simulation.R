rm(list=ls())

library(tidyverse)
library(lubridate)
library(statmod)


# Retrieve function
source(file = "R/00_sim_function.R")

# Input to function is farm name and number of simulation to run

result <- run_simulation("C1",30)

sim_validation <- result[[1]]
sim_IBM <- result[[2]]
sim_ODE <- result[[3]]

save(sim_validation,file = "results/validation_C1.RData")
save(sim_IBM,file = "results/IBM_C1.RData")
save(sim_ODE,file = "results/ODE_C1.RData")
