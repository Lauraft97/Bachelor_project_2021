# 07 Parameter calculations

rm(list = ls())

library(tidyverse)
library(statmod)


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

#Mean of positive EPG values
EPG_data %>% filter(EPG > 0) %>% summarize(EPG_mean = mean(EPG))

#Saving with values > 0
a <- EPG_data %>% filter(EPG > 0)

# Inverse Gaussian distribution 
tibble(x = rinvgauss(273,mean = 1.52, shape = 0.2)) %>% ggplot(mapping = aes(x = x)) + 
  geom_density()

# Combining data
data <- bind_cols(a,tibble(x = rinvgauss(273,mean = 1.52, shape = 0.35)))

# Comparing distribution and distribution of egg in data
data %>% filter(EPG < 20) %>%  
  ggplot() + geom_density(mapping = aes(x = EPG)) +
  geom_density(mapping = aes(x = x,
                             col = "red"))

