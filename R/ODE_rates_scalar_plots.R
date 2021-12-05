# ODE rates scalar plots

library(tidyverse)


color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
windowsFonts(`Lucida Bright` = windowsFont("Lucida Bright"))
Sys.setlocale("LC_ALL","English")


# Egg excretion scalar ----------------------------------------------------

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
  geom_line(col = color_scheme[1]) +
  theme_bw(base_size = 12,
           "Lucida Bright")  +
  labs(x = "Sick period [days]",
       y = "Scalar",
       title = "Egg excretion",
       subtitle = "Sick period scalar") + 
  scale_x_continuous(breaks = c(60,90,240,300,500))


ggsave(filename = "results/figures/Final_figures/egg_scaling_factor.png")  



# Transmission scalar -----------------------------------------------------
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Corrected ground temperature [\u00B0C]",
       y = "Scalar",
       title = expression(paste("Transmission ",lambda)),
       subtitle = " Temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/Final_figures/trans_temp_scalar.png")



# Death rate scalar -------------------------------------------------------
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = 1-pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Corrected ground temperature [\u00B0C]",
       y = "Scalar",
       title = expression(paste("Death rates ",mu)),
       subtitle = " Temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/Final_figures/mu_scalar.png")



