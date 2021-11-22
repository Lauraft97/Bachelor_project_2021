# 07 Parameter calculations

rm(list = ls())

library(tidyverse)
library("RColorBrewer")
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]


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

# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm

fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE))

#a <- fluke_data %>% filter(dEPG == TRUE)

#a %>% group_by(Farm) %>% distinct(UniqueID) %>% summarise(n = n())
#fluke_diag %>% filter(Diag == TRUE) %>% group_by(Farm) %>% distinct(UniqueID) %>% summarise(n = n())

#Cohort size
#fluke_data %>% group_by(Farm) %>% distinct(UniqueID) %>%  summarise(n = n())

Farm_name <- c("C1","C2","O1","O2")

Visit_name <- c("A","B","C","D","E","F","G")

n_Infected <- matrix(nrow = 4,
                     ncol = 7,
                     dimnames = list(Farm_name,Visit_name))

n_cow <- matrix(nrow = 4,
                     ncol = 7,
                     dimnames = list(Farm_name,Visit_name))

n_days <- matrix(nrow = 4,
                 ncol = 7,
                 dimnames = list(Farm_name,Visit_name))


for(k in 1:4){
  
Farm_data <- fluke_diag %>% filter(Farm == Farm_name[k])  

  for(i in 1:7){
  
  ID <- Farm_data %>% filter(Visit == Visit_name[i] & Diag == T) %>% 
                    select(UniqueID) %>% 
                    pull()
  
  n_Infected[k,i] <- length(ID)
  
  n_cow[k,i] <- Farm_data %>% filter(Visit == Visit_name[i]) %>% 
    distinct(UniqueID) %>% nrow()
  
  Farm_data <- Farm_data %>% filter(!(UniqueID %in% ID))
    
  }

dates <- append(as.Date("2014-08-01"), Farm_data %>% filter(Farm == Farm_name[k]) %>% 
                  arrange(Date) %>%  
                  distinct(Date) %>% pull() %>% as.Date()) 

n_days[k,] <- diff(dates)
  
}


# Infected per visit per cow susceptible in cohort
I_pr_cow <- n_Infected / n_cow

#I_pr_cow[,-1]

I_pr_cow_pr_day <- I_pr_cow / n_days

#I_pr_cow_pr_day[,-1]*300

#Probabilities for infection

metac <- M

x <- 10^-6.5

cow_prob_mean <- seq(1,length(metac))

for(i in 1:length(metac)){
  
  M_1 <- metac[i]
  grazing <- runif(300,0,1)
  prob <- 1-exp(-grazing*M_1*x)
  
  cow_prob_mean[i] <- mean(prob)
}


visits <- seq(1,6)

visits[1] <- mean(cow_prob_mean[1:95])
visits[2] <- mean(cow_prob_mean[96:195])
visits[3] <- mean(cow_prob_mean[196:276])
visits[4] <- mean(cow_prob_mean[277:345])
visits[5] <- mean(cow_prob_mean[346:474])
visits[6] <- mean(cow_prob_mean[475:620])

O1 <- I_pr_cow_pr_day[3,-1]
C1 <- I_pr_cow_pr_day[1,-1]
O2 <- I_pr_cow_pr_day[4,-1]
C2 <- as.numeric(I_pr_cow_pr_day[2,-1])

#Plot
sim_transmission <- tibble(dates = c("B","C","D","E","F","G"),
                           simulation = visits,
                           C1 = as.numeric(I_pr_cow_pr_day[1,-1]),
                           C2 = as.numeric(I_pr_cow_pr_day[2,-1]),
                           O1 = as.numeric(I_pr_cow_pr_day[3,-1]),
                           O2 = as.numeric(I_pr_cow_pr_day[4,-1]))

sim_transmission %>% ggplot(mapping = aes(x = dates,
                                          group = 4)) +
  geom_point(aes(y = simulation, col ="Simulation"), shape = 4, size = 3) +
  geom_point(aes(y = C1, col = "C1"))+ 
  geom_line(aes(y = C1), color = color_scheme[4])+
  geom_point(aes(y = C2,col ="C2"))+
  geom_line(aes(y = C2),col = color_scheme[5])+
  geom_point(aes(y = O1, col = "O1"))+
  geom_line(aes(y = O1),col = color_scheme[6])+
  geom_point(aes(y = O2, col="O2"))+
  geom_line(aes(y = O2),col = color_scheme[8])+
  theme(legend.position = "bottom") +
  theme_bw(base_size = 8) +
  scale_color_manual(name="Farm name",
                     breaks=c("Simulation", "C1", "C2", "O1","O2"),
                     values=c("Simulation"= 1, "C1" = color_scheme[4],
                              "C2" = color_scheme[5],"O1" = color_scheme[6],
                              "O2" = color_scheme[8]),
                     guide = guide_legend(override.aes = list(shape = c(4, 16, 16, 16, 16),
                                                              size = c(2,2,2,2,2))))+
  labs(x = "Visit",
       y = "Transmission proberbility",
       title = "Comparing simulated transmission probabilities with farm data",
       subtitle = "Scaling factor = 10^(-7)")

ggsave(filename = "results/figures/03_trans_prob.png",
       width = 10*1.5, 
       height = 6.5, 
       units = "cm",
       dpi = 150) 

