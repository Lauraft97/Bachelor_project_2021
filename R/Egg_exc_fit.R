rm(list = ls())

library('tidyverse')
library('pscl')

windowsFonts(`Lucida Bright` = windowsFont("Lucida Bright"))
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]

fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")

fit_data <- fluke_data %>%
  filter(!is.na(EPG) & EPG < 20) %>%
  mutate(EggCount = EPG*5) %>%
  select(Farm, UniqueID, Group, Date, EggCount)


fit_distr <- zeroinfl(EggCount ~ 1 | Farm, data = fit_data, dist = "negbin")

mu_fit <- exp(fit_distr$coefficients[[1]])

theta_fit <- fit_distr$theta


## ggplot------------------------------------------
nb <- tibble(egg = rnbinom(1e6, theta_fit, mu=mu_fit)) %>% 
  mutate(type = "fit")
nb <- nb %>% filter(egg > 0)
  
obs <- tibble(egg = fit_data %>% filter(EggCount>0 & EggCount < 100) %>% pull(EggCount)) %>% 
  mutate(type = "obs")

ecdf <- bind_rows(nb,obs)

ggplot(ecdf, aes(egg, colour = type)) +
  stat_ecdf() +
  scale_color_manual(values = c(color_scheme_2[2],
                                color_scheme_2[5])) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Index",
       y = "% of eggs",
       title = "ECDF function for excreation of eggs",
       subtitle = "Fit on mean of the four farms") +
  xlim(0,60)
