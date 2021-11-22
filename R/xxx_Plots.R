color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

library(patchwork)


# Other plots -------------------------------------------------------------



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








# O2 - SEI ------------------------------------------------------

S_O2 <- results_IBM_S %>% ggplot(aes(x = timestep,
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



E_O2 <- results_IBM_E %>% ggplot(aes(x = timestep,
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


I_O2 <- results_IBM_I %>% ggplot(aes(x = timestep,
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

S_O2/E_O2/I_O2 + plot_annotation(title = "Farm O2")








# Validation --------------------------------------------------------------

# Load data 
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")

# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm

fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE))

sick_sim <- results_validation_total


sick_sim <- sick_sim %>% mutate(Visit = case_when(Visit_day_no == visit_days_n[1] ~ "B",
                                                  Visit_day_no == visit_days_n[2] ~ "C",
                                                  Visit_day_no == visit_days_n[3] ~ "D",
                                                  Visit_day_no == visit_days_n[4] ~ "E",
                                                  Visit_day_no == visit_days_n[5] ~ "F",
                                                  Visit_day_no == visit_days_n[6] ~ "G")) %>% 
  
  ungroup() %>% 
  select(Visit,Count,Simulation)


sick_data <- fluke_diag %>% filter(Farm == "C2", Diag == T) %>% group_by(Visit) %>% 
              summarise(Count = n()) %>% slice(-1) %>% mutate(Simulation = "Data")

plot_data <- bind_rows(sick_sim,sick_data)


plot_data %>%  ggplot(aes(x = Visit,
                          y = Count,
                          color = Sim)) + geom_point() +
  labs(title = "Farm O2, 10 simulations vs real data")


plot_data %>%  ggplot(aes(x = Visit,
                          y = Count,
                          group = Simulation)) + 
  geom_point(aes(shape = Simulation, 
                 color = Simulation,
                 size = Simulation)) +
  geom_line(aes(color = Simulation)) + 
  scale_shape_manual(values=c(rep(16,10),4)) +
  scale_color_manual(values = c(rep(color_scheme[5],10),"black")) +
  scale_size_manual(values=c(rep(1,10),3)) +
  theme_bw() +
  labs(title = "Farm C2 - Simulations and farm data")





# ODE ---------------------------------------------------------------------

results_ODE_E1 %>% ggplot(aes(x = 1:time,
                               y = mean)) +
  geom_line() +
  geom_line(aes(y = lower.ci,
                color = "Lower"),
            linetype = 2) +
  geom_line(aes(y = upper.ci,
                color = "Lower"),
            linetype = 2) +
  labs(title = "E1 snail mean + 95 % CI")


results_ODE_M %>% ggplot(aes(x = 1:time,
                              y = mean)) +
  geom_line() +
  geom_line(aes(y = lower.ci,
                color = "Lower"),
            linetype = 2) +
  geom_line(aes(y = upper.ci,
                color = "Lower"),
            linetype = 2) +
  labs(title = "Metacercariae mean + 95 % CI")






# Validation within groups


sick_sim_group <- results_validation %>% mutate(Visit = case_when(Visit_day_no == visit_days_n[1] ~ "B",
                                                  Visit_day_no == visit_days_n[2] ~ "C",
                                                  Visit_day_no == visit_days_n[3] ~ "D",
                                                  Visit_day_no == visit_days_n[4] ~ "E",
                                                  Visit_day_no == visit_days_n[5] ~ "F",
                                                  Visit_day_no == visit_days_n[6] ~ "G")) 



inf_count_2 <- inf_count %>% mutate(Simulation = "Data") %>%  filter(model_group == 2, Farm == "C2") 


plot_data_2 <- bind_rows(sick_sim_group,inf_count_2)
  

plot_data_2 %>%  ggplot(aes(x = Visit,
                          y = Count,
                          group = Simulation)) + 
  geom_point(aes(shape = Simulation, 
                 color = Simulation,
                 size = Simulation)) +
  geom_line(aes(color = Simulation)) + 
  scale_shape_manual(values=c(rep(16,10),4)) +
  scale_color_manual(values = c(rep(color_scheme[5],10),"black")) +
  scale_size_manual(values=c(rep(1,10),3)) +
  theme_bw() +
  labs(title = "Farm C2 - Simulations and farm data")


















