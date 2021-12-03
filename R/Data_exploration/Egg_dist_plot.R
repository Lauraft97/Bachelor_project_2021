#Egg distrubution Plot
rm(list  = ls())
library(tidyverse)

egg_mu_scaled <- 2.773*(12000/5)
egg_theta_distr <- 0.287
draws <- 1000

a <-  tibble(eggs = rnbinom(draws,egg_theta_distr,mu = egg_mu_scaled))

a %>% ggplot(aes(x = eggs))+
  geom_histogram(aes(y = ..density..), alpha = 0.5,
                 position ="identity",
                 binwidth = 3000) +
  geom_density() #+ 
  #xlim(0,60000)


a %>% ggplot(aes(x = eggs))+
  geom_freqpoly(bins = 150) +
  xlim(0,60000)

# a %>% filter(eggs == 0) %>% 
#   summarise(n()) %>% 
#   pull()


