library(tidyverse)
library(patchwork)



FarmID = "O2"



source(file = "R/02C_Farm_info.R")



load(paste0("results/ODE_",FarmID,".Rdata"))



color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
color_df <- as.data.frame(cbind(var = c("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2"),
                                color_scheme))



Farm_col <- color_df %>% filter(var == FarmID) %>% select(color_scheme) %>% pull()




Eggs %>% filter(variable == "Egg") %>% ggplot(aes(x = date,
                                                  y = median,
                                                  color = Farm)) +
  geom_line()


Eggs %>% filter(variable == "new_Egg") %>% ggplot(aes(x = date,
                                                      y = median,
                                                      color = Farm)) +
  geom_line()


Eggs %>% filter(variable == "Egg" & Farm == "O2" & date > as.Date("2017-08-01")) %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = median, color = "median")) +
  geom_line(aes(y = Q1, color = "Quantile"),
            alpha = 0.5) +
  geom_line(aes(y = Q3, color = "Quantile"),
            alpha = 0.5) + theme_bw()






Snails %>% filter(Farm == "O1" & variable %in% c("S","E1")) %>% ggplot(aes(x = date,
                                                                                  y = median,
                                                                                  color = variable)) +
  geom_line() + theme_bw()


Snails %>% filter(variable == "I") %>% ggplot(aes(x = date,
                                                      y = median,
                                                      color = Farm)) +
  geom_line()

Eggs %>% filter(variable == "Egg") %>% ggplot(aes(x = date,
                                                      y = median,
                                                      color = Farm)) +
  geom_line()


Metacercariae %>% ggplot(aes(x = date,
                            y = median,
                            color = Farm)) +
  geom_line()

