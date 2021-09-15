# 03 Data preparation for IBM

rm(list = ls())

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")

# Adding variables to the data set. Farm and group gets numerical values
fluke_data_IBM <- fluke_data %>% 
  mutate(Farm_ID = case_when(Farm == "C1" ~ 1,
                             Farm == "C2" ~ 2,
                             Farm == "O1" ~ 3,
                             Farm == "O2" ~ 4),
         Group_ID = case_when(Group == "Calf" ~ 1,
                              Group == "Heifer" ~ 2,
                              Group == "Primiparous" ~ 3,
                              Group == "Multiparous" ~ 4)) %>% 
  select(UniqueID, Farm_ID, Group_ID) %>% 
  distinct(UniqueID,.keep_all = TRUE)


# Save data ---------------------------------------------------------------

write_tsv(fluke_data_IBM,"data/03_fluke_data_IBM.tsv")



