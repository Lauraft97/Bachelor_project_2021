# 04 IBM for life cycle stages in cows

rm(list = ls())

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Load data ---------------------------------------------------------------
fluke_data_IBM <- read_tsv(file = "data/03_fluke_data_IBM.tsv")


# Risk of infection on different farms and within a given group ------------


