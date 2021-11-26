rm(list = ls())

library(epiR)


load("results/sensitivity_O1_Anna.RData")
load("results/sensitivity_LFT.RData")
load("results/sensitivity_LFT_2.RData")
load("results/sensitivity_SSL.RData")
load("results/sensitivity_SSL_2.RData")
load("results/sensitivity_SSL_3.RData")
load("results/sensitivity_ASBJ.RData")
load("results/sensitivity_ASBJ_15.RData")

Anna_2 <- sensitivity_ASBJ

results <- rbind(sensitivity_Anna,sensitivity_LFT,sensitivity_SSL, sensitivity_ASBJ, 
                 sensitivity_LFT_2, sensitivity_SSL_2, sensitivity_SSL_3, 
                 Anna_2)

results <- as.data.frame(results)

results_scaled <- results

epi.prcc(results)

pcor(results[,c(7,10)])

library(GGally)

ggpairs(results) + theme_bw()
