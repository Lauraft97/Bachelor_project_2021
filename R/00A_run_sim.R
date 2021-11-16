rm(list=ls())

library(tidyverse)
library(lubridate)
library(statmod)


# Retrieve function
source(file = "R/00_sim_function.R")

# Input to function is farm name and number of simulation to run


run_simulation("O2",1)