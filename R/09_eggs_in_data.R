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

# Filtering on all cows that have a positive faecal egg count
FEC_pos_cows <- fluke_data %>% filter(dEPG =TRUE) %>% distinct(UniqueID) %>% pull()
  
EPG_data <- fluke_data %>% filter(UniqueID %in% FEC_pos_cows) %>% 
            select(Farm,UniqueID,Visit,EPG,dEPG) %>% 
            arrange(UniqueID,Visit) %>% 
            drop_na()


EPG_data %>% ggplot(mapping = aes(x = Visit,
                                  y = EPG)) +
             geom_point(mapping = aes(col = UniqueID)) +
             facet_wrap(~Farm, scales = "free") +
             theme(legend.position = "none") 





