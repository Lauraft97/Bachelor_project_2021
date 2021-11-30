rm(list = ls())

library(epiR)


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
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]




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



library(GGally)

ggpairs(results) + theme_bw()
