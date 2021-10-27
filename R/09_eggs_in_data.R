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
EPG_pos_cows <- fluke_data %>% filter(dEPG == TRUE) %>% 
                distinct(UniqueID) %>% pull()

zero_EPG_cows <- fluke_data %>% filter((EPG == 0 | dSerum == T | dCopro == T) & 
                                       !(UniqueID %in% EPG_pos_cows)) %>% 
  distinct(UniqueID) %>% pull()


  
EPG_data <- fluke_data %>% filter(UniqueID %in% EPG_pos_cows) %>% 
            select(Farm,UniqueID,Visit,EPG,dEPG, dSerum, dCopro) %>% 
            arrange(UniqueID,Visit) %>% 
            drop_na()

EPG_data_pos <- EPG_data


a %>% ggplot(mapping = aes(x = Visit,
                           y = Serum)) +
             geom_point(aes(color = dSerum)) +
             facet_wrap(~Farm, scales = "free") +
             theme(legend.position = "none")

as.integer(EPG_data_pos$EPG)


require(pscl)

m1 <- zeroinfl(EPG ~ , data = EPG_data_pos)



#ECDF plot of the faecal egg count
EPG_data %>% filter(EPG > 0) %>% ggplot(aes(EPG)) + stat_ecdf(geom = "point")


# Inverse gaussian distribution

values_1 <- matrix(data = NA, nrow = 10000, ncol = 4)
lambda <- seq(0.1,1.3,0.4)


for(i in c(1:4)){
  
  values[,i] <- round(rinvgauss(10000,mean = 1.52, shape = lambda[i]),2)
  #values[,i] <- colMeans(Gauss)
}


library(fitdistrplus)
library(FAdist)
library(weibullness)

#x_all <- EPG_data %>% filter(EPG > 0) %>% dplyr::select(EPG) %>% pull()
x <- EPG_data %>% filter(EPG > 0) %>% dplyr::select(EPG) %>% pull()

fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
fit.IG <- fitdist(x, distr = "invgauss", start = list(mean = 1.52, shape = 0.5))
fit.W <- fitdist(x, "weibull")
fit.W3 <- weibull.mle(x)


summary(fit.gamma)
summary(fit.IG)
fit.W3


Gamma = rgamma(10000, shape = 0.7480860, rate = 0.4985634)
IG = rinvgauss(10000, mean = 1.500597, shape = 0.643915)
W_3 <- rweibull3(10000, shape = 0.65, scale = 1, thres = 0.1)

ggplot() + geom_density(aes(Gamma, col = "Gamma")) +
  geom_density(aes(x_all, col = "data")) +
  geom_density(aes(IG, col = "InvGauss")) +
  geom_density(aes(W_3, col = "Weibull3")) +
  xlim(0,10) +
  labs(x = "Eggs")


ggplot() + stat_ecdf(aes(Gamma, color = "Gamma"), geom = "line") +
  stat_ecdf(aes(x, color = "data")) +
  stat_ecdf(aes(IG, color = "Inv_Gauss")) +
  stat_ecdf(aes(W_3, color = "Weibull3")) +
  xlim(0,10) +
  labs(x = "Eggs")



b <- EPG_data %>% filter(EPG > 0) %>% select(EPG) %>% pull()

colors <- c("0.1"= "red", "0.5" = "blue", "0.9" = "purple", "1.3" = "green", "data" = "black")

  ggplot() +
  stat_ecdf(aes(values[,1],
                color = "0.1"), geom = "line") +
  stat_ecdf(aes(values[,2],
                color = "0.5"), geom = "line") +
  stat_ecdf(aes(values[,3],
                color = "0.9"), geom = "line") +
  stat_ecdf(aes(values[,4], 
                color = "1.3"), geom = "line") + 
  stat_ecdf(aes(b,
                color = "data")) +  
    labs(x = "Eggs pr 5 gram",
         y = "Density",
         title = "ECDF for inverse Gaussian distribution",
         subtitle = "Different shape parameters",
         color = "Lambda") +   
  scale_color_manual(values = colors) +
  xlim(0,7.5)



#Mean of positive EPG values
EPG_data %>% filter(EPG > 0) %>% summarize(EPG_mean = mean(EPG))

#Saving with values > 0
a <- EPG_data %>% filter(EPG > 0)

# Inverse Gaussian distribution 
tibble(x = rinvgauss(273,mean = 1.52, shape = 1)) %>% ggplot(mapping = aes(x = x)) + 
  geom_density()

# Combining data
data <- bind_cols(a,as.tibble(values))


ggplot() + geom_density(aes(values[,2])) + geom_density(aes(values[,1]), col = "blue") + 
  geom_density(aes(values[,3]), col = "green") +
  geom_density(aes(values[,4]), col = "purple") +
  geom_density(aes(a$EPG), col = "red") +
  xlim(0,2)

ggplot() + geom_density(aes(values[,2])) + geom_density(aes(data$EPG), col = "red")

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
                           col = "0.7")) +
  xlim(0,2)


