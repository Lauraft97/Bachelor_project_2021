rm(list = ls())

library(tidyverse)
library(lubridate)
library(statmod)
library(patchwork)


# Population --------------------------------------------------------------

load("data/Simulations_SSL/Pop_sim_1.RData")
Pop_sim1 <- Pop_sim
load("data/Simulations_SSL/Pop_sim_2.RData")
Pop_sim2 <- Pop_sim


mean_sim1 <- rep(0,633)
sd_sim1 <- rep(0,633)
mean_sim2 <- rep(0,633)
sd_sim2 <- rep(0,633)

for (i in 1:633){
  mean_sim1[i] <- mean(Pop_sim1[,i])
  mean_sim2[i] <- mean(Pop_sim2[,i])
  
  sd_sim1[i] <- sd(Pop_sim1[,i])
  sd_sim2[i] <- sd(Pop_sim2[,i])
}

p1 <- ggplot(mapping = aes(x = 1:633)) +
  geom_line(aes(y = mean_sim1), colour = "red") +
  geom_line(aes(y = mean_sim1+sd_sim1),
                linetype = 2) +
  geom_line(aes(y = mean_sim1-sd_sim1),
            linetype = 2) +
  ylim(290,340) +
  labs(title = "Simulation 1",
       subtitle = "Oldest cow init: 3.75 years. \n Slaughter random between 3.75 and 6 years")
  #ggtitle("Simulation 1")

p2 <- ggplot(mapping = aes(x = 1:633)) +
  geom_line(aes(y = mean_sim2), colour = "blue") +
  geom_line(aes(y = mean_sim2+sd_sim2),
            linetype = 2) +
  geom_line(aes(y = mean_sim2-sd_sim2),
            linetype = 2) +
  ylim(290,340) +
  labs(title = "Simulation 2",
       subtitle = "Oldest cow init: 3.5 years. \n Slaughter random between 3.5 and 6 years")

p1 + p2


# Births ------------------------------------------------------------------
load("data/Simulations_SSL/Births_sim_1.RData")
Births_sim1 <- Births_sim
load("data/Simulations_SSL/Births_sim_2.RData")
Births_sim2 <- Births_sim


mean_births_sim1 <- rep(0,633)
sd_births_sim1 <- rep(0,633)
mean_births_sim2 <- rep(0,633)
sd_births_sim2 <- rep(0,633)

for (i in 1:633){
  mean_births_sim1[i] <- mean(Births_sim1[,i])
  mean_births_sim2[i] <- mean(Births_sim2[,i])
  
  sd_births_sim1[i] <- sd(Births_sim1[,i])
  sd_births_sim2[i] <- sd(Births_sim2[,i])
}

boxplot(mean_births_sim1, mean_births_sim2)



