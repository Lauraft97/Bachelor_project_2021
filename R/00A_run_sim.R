rm(list=ls())

library(tidyverse)
library(lubridate)
library(statmod)


# Retrieve function
source(file = "R/00_sim_function_16.nov.R")

# Input to function is farm name and number of simulation to run
run_simulation(FarmID = "O2", nruns = 3)


save(sim_validation,file = paste0("results/validation_",FarmID,".Rdata"))
save(sim_IBM,file = paste0("results/IBM_",FarmID,".Rdata"))
save(sim_ODE,file = paste0("results/ODE_",FarmID,".Rdata"))
