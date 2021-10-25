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


#ECDF plot of the faecal egg count
EPG_data %>% filter(EPG > 0) %>% ggplot(aes(EPG)) + stat_ecdf(geom = "point")


# Inverse gaussian distribution

values <- matrix(data = NA, nrow = 273, ncol = 4)
lambda <- seq(0.1,0.7,0.2)

for(i in c(1:4)){
  values[,i] <- round(rinvgauss(273,mean = 1.52, shape = lambda[i]),2)
}



EPG_data %>% filter(EPG > 0) %>% ggplot() + 
  stat_ecdf(aes(EPG), geom = "point") +
  stat_ecdf(aes(values[,1]), color = "red", geom = "point") +
  stat_ecdf(aes(values[,2]), color = "green", geom = "point") +
  stat_ecdf(aes(values[,3]), color = "blue", geom = "point") +
  stat_ecdf(aes(values[,4]), color = "purple", geom = "point")



#Mean of positive EPG values
EPG_data %>% filter(EPG > 0) %>% summarize(EPG_mean = mean(EPG))

#Saving with values > 0
a <- EPG_data %>% filter(EPG > 0)

# Inverse Gaussian distribution 
tibble(x = rinvgauss(273,mean = 1.52, shape = 1)) %>% ggplot(mapping = aes(x = x)) + 
  geom_density()

# Combining data
data <- bind_cols(a,as.tibble(values))

# Comparing distribution and distribution of egg in data
data %>%  
  ggplot() + geom_density(mapping = aes(x = EPG)) +
  geom_density(mapping = aes(x = V1,
                             col = "0.1")) +
  geom_density(mapping = aes(x = V2,
                             col = "0.3")) +
  geom_density(mapping = aes(x = V3,
                           col = "0.5")) +
  geom_density(mapping = aes(x = V4,
                           col = "0.7"))


