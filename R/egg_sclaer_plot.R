# Plot of egg excreted scaling factor 

rm(list = ls())

library(RColorBrewer)
library(tidyverse)


# Color scheme
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

# Simulated data
linear <- function(x){(y = (1/(90-60))*x-2)
  return(y)}
  
exponential <- function(x){y = exp(-(0.05*(x-(8*30))))
                           return(y)}

y_lin <- linear(seq(60,90,1))
steady <- rep(1,240-90)
y_exp <- exponential(seq(241,500))

egg_scaler <- tibble(sick_period = seq(60,500,1),
                     scaler = c(y_lin, steady, y_exp))


egg_scaler %>% ggplot(aes(x = sick_period,
                          y = scaler)) +
  geom_vline(xintercept = c(90,240),
             linetype = "dashed") +
  geom_line(col = color_scheme[1]) +
  theme_bw()  +
  labs(x = "Sick Period [days]",
       y = "Scaling factor",
       title = "Eggs excretion scaling factor")



  
