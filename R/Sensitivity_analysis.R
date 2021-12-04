rm(list = ls())

library(epiR)
library(tidyverse)
library(GGally)
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]


load("results/sensitivity_O1_Anna.RData")
load("results/sensitivity_LFT.RData")
load("results/sensitivity_LFT_2.RData")
load("results/sensitivity_SSL.RData")
load("results/sensitivity_SSL_2.RData")
load("results/sensitivity_SSL_3.RData")
load("results/sensitivity_SSL_4.RData")
load("results/sensitivity_SSL_5.RData")
load("results/sensitivity_ASBJ.RData")
Anna_2 <- sensitivity_ASBJ
load("results/sensitivity_ASBJ_15.RData")


results <- rbind(sensitivity_Anna,sensitivity_LFT,sensitivity_SSL, sensitivity_ASBJ, 
                 sensitivity_LFT_2, sensitivity_SSL_2, sensitivity_SSL_3, 
                 Anna_2,sensitivity_SSL_4,sensitivity_SSL_5)

results <- as.data.frame(results)


# PRCC --------------------------------------------------------------------

windowsFonts(`Lucida Bright` = windowsFont("Lucida Bright"))

PRCC <- epi.prcc(results)
PRCC <- tibble(PRCC)
PRCC <- PRCC %>% mutate(variable = colnames(results[,-10]) )

PRCC %>% filter(p.value < 0.05) %>% 
  ggplot(aes(x = variable, 
             y = est,
             fill = variable)) +
  geom_bar(stat="identity",
           color = "gray40") + 
  geom_label(aes(label=round(est,2),
                 family = "Lucida Bright")) + 
  scale_fill_manual(values = c(color_scheme_2[1],
                               color_scheme_2[3],
                               color_scheme_2[5],
                               color_scheme_2[7],
                               color_scheme_2[9],
                               color_scheme_2[11])) +
  theme_bw(base_family = "Lucida Bright",
           base_size = 12) +
  labs(title = "Partial rank correlation coefficients - Infected cattle at the last time point",
       subtitle = "p-values < 0.05",
       x = "",
       y = "PRCC") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12)) + 
  scale_x_discrete(limit = c("sla_prob",
                             "delta_snail_sa",
                             "Snail_pop0",
                             "mu_M_sa",
                             "mu_S_sa",
                             "gamma_S_sa"),
                   labels = c("Slaughter probability",
                              expression(delta[S]),
                              "Baseline snail population", 
                              expression(mu[M]),
                              expression(mu[S]),
                              expression(gamma[S]))) 

ggsave("results/figures/Final_figures/PRCC.png")


# Scatterplot -------------------------------------------------------------

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  p <- cor.test(x, y)$p.value
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  if(p >= 0.05) txt <- paste0("Corr  \n", txt)
  if(p < 0.05 & p >= 0.01) txt <- paste0("Corr  \n", txt,"*")
  if(p < 0.01 & p >= 0.001) txt <- paste0("Corr  \n",txt,"**")
  if(p < 0.001) txt <- paste0("Corr  \n",txt,"***")
  text(0.5, 0.5, txt)
}


# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 16, col = color_scheme[1])
}

png("results/figures/Final_figures/scatter.png",width = 700)
# Create the plots
pairs(results, 
      lower.panel = upper.panel,
      upper.panel = panel.cor,
      labels = c(expression(lambda[ES]),
                 expression(mu[Egg]),
                 expression(delta[S]),
                 expression(gamma[S]),
                 expression(mu[S]),
                 expression(mu[M]),
                 "Slaughter \n probability",
                 expression(paste("egg ",mu," scaled")),
                 "Baseline \n snail pop ",
                 "End \n infected"),
      cex.labels=1.15,gap=0.5)
dev.off()

