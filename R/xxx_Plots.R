color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

library(patchwork)

# Temperature dependent exponential distribution CDF
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  #scale_color_brewer(palette = "Set2") +
  theme_bw(base_size = 8) +
  labs(x = "Corrected mean temperature [\u00B0C]",
       y = "Scalar",
       title = "Transmission temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/trans_temp_scalar.png",
       width = 10, 
       height = 6.5, 
       units = "cm",
       dpi = 150)  


# Temperature dependent exponential distribution CDF
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = 1-pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  #scale_color_brewer(palette = "Set2") +
  theme_bw(base_size = 8) +
  labs(x = "Corrected mean temperature [\u00B0C]",
       y = "Scalar",
       title = "Death rate temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/mu_scalar.png",
       width = 10, 
       height = 6.5, 
       units = "cm",
       dpi = 150)  







# Results SEI -----------------------------------------------------------------

# O2 - SEI

S <- results_IBM_S %>% ggplot(aes(x = timestep,
                                  y = mean,
                                  color = Group)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[2], 
                              color_scheme[7], 
                              color_scheme[3]),
                     labels = c("Calf", "Heifer", "Cow")) +
  theme_bw(base_size = 8) +
  labs(x = "",
       y = "Cattle [#]",
       title = "Susceptible") +
  theme(axis.text.x=element_blank())+ 
  geom_vline(xintercept = 624,
             color = "gray60",
             linetype = "dashed") +
  theme(legend.position = "none")



E <- results_IBM_E %>% ggplot(aes(x = timestep,
                             y = mean,
                             color = Group)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[2], 
                              color_scheme[7], 
                              color_scheme[3]),
                     labels = c("Calf", "Heifer", "Cow")) +
  theme_bw(base_size = 8) +
  labs(x = "",
       y = "Cattle [#]",
       title = "Exposed") +
  geom_vline(xintercept = 624,
             color = "gray60",
             linetype = "dashed") +
  theme(axis.text.x=element_blank())


I <- results_IBM_I %>% ggplot(aes(x = timestep,
                                  y = mean,
                                  color = Group)) +
  geom_line() + 
  scale_color_manual(values=c(color_scheme[2], 
                              color_scheme[7], 
                              color_scheme[3]),
                     labels = c("Calf", "Heifer", "Cow")) +
  theme_bw(base_size = 8) +
  labs(x = "Time [Days]",
       y = "Cattle [#]",
       title = "Infected") + 
  geom_vline(xintercept = 624,
             color = "gray60",
             linetype = "dashed") +
  theme(legend.position = "none")

S/E/I + plot_annotation(title = "Farm O2")
















# Validation --------------------------------------------------------------

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

sick_sim <- results_validation_total

sick_sim %>% ggplot(aes(x = Visit_day_no,
                        y = n)) + geom_line()


sick_data <- fluke_diag %>% filter(Farm == "O2", Diag == T) %>% group_by(Visit) %>% summarise(n = n()) %>% slice(-1) 

sick_data %>%  ggplot(aes(x = Visit,
                          y = n)) + geom_point() + geom_point(aes(y = sick_sim$mean,
                                                                  col = "simulation")) 


results_validation_total %>% ggplot(aes(x = Visit_day_no,
                                        y = Count,
                                        col = Simulation)) +
  geom_point() 



