library('tidyverse')
library('pscl')


fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")

data <- fluke_data %>%
	filter(!is.na(EPG)) %>%
	mutate(EggCount = EPG*5) %>%
	select(Farm, UniqueID, Group, Date, EggCount) %>%
	# ZINB is extremely sensitive to the two extremely high counts - we should follow up with Nao:
	filter(EggCount < 100)

data %>% count(Farm, EggCount) %>% print(n=Inf)

fm_zinb <- zeroinfl(EggCount ~ Farm | Farm, data = data, dist = "negbin")

fm_zinb_mean <- zeroinfl(EggCount ~ 1 | Farm, data = data, dist = "negbin")

zeroinfl(EggCount ~ 1 | 1, data = data, dist = "negbin")

dis_mean <- exp(1.025)
theta_mean <- fm_zinb_mean$theta


farm_means <- exp(coef(fm_zinb)["count_(Intercept)"] + c(count_FarmC1=0,coef(fm_zinb)[str_c("count_Farm", c("C2","O1","O2"))]))
theta <- fm_zinb$theta
farm_prev <- 1 - plogis(coef(fm_zinb)["zero_(Intercept)"] + c(zero_FarmC1=0,coef(fm_zinb)[str_c("zero_Farm", c("C2","O1","O2"))]))


## C1:
nb <- rnbinom(1e6, theta, mu=farm_means[1]) %>% `[`(., .>0)
obs <- data %>% filter(Farm=="C1", EggCount>0) %>% pull(EggCount)
plot.ecdf(nb, verticals=TRUE)
plot.ecdf(obs, col="red", add=TRUE, verticals=TRUE)

## C2:
nb <- rnbinom(1e6, theta, mu=farm_means[2]) %>% `[`(., .>0)
obs <- data %>% filter(Farm=="C2", EggCount>0) %>% pull(EggCount)
plot.ecdf(nb, verticals=TRUE)
plot.ecdf(obs, col="red", add=TRUE, verticals=TRUE)

## O1:
nb <- rnbinom(1e6, theta, mu=farm_means[3]) %>% `[`(., .>0)
obs <- data %>% filter(Farm=="O1", EggCount>0) %>% pull(EggCount)
plot.ecdf(nb, verticals=TRUE)
plot.ecdf(obs, col="red", add=TRUE, verticals=TRUE)

## O2:
nb <- rnbinom(1e6, theta, mu=farm_means[4]) %>% `[`(., .>0)
obs <- data %>% filter(Farm=="O2", EggCount>0) %>% pull(EggCount)
plot.ecdf(nb, verticals=TRUE)
plot.ecdf(obs, col="red", add=TRUE, verticals=TRUE)


## Using same mean:
nb <- rnbinom(1e6, theta_mean, mu=dis_mean) %>% `[`(., .>0)
obs <- data %>% filter(EggCount>0) %>% pull(EggCount)
plot.ecdf(nb, verticals=TRUE)
plot.ecdf(obs, col="red", add=TRUE, verticals=TRUE)


## ggplot------------------------------------------
nb <- tibble(x = rnbinom(1e6, theta_mean, mu=dis_mean) %>% `[`(., .>0)) %>% 
  mutate(type = "fit")
obs <- tibble(x = data %>% filter(EggCount>0) %>% pull(EggCount)) %>% 
  mutate(type = "obs")
ecdf <- bind_rows(nb,obs)


ggplot(ecdf, aes(x, colour = type)) +
  stat_ecdf() +
  scale_color_manual(values = c("black", "red")) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  labs(x = "Index",
       y = "% of eggs",
       title = "ECDF function for excreation of eggs",
       subtitle = "Fit on mean of the four farms")
