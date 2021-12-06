# IBM + validation results plot
rm(list=ls())

library(cowplot)
source(file = "R/06_plot_functions.R")
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]




# IBM data preparation function -------------------------------------------

# Plot SEI total function --------------------------------------------------
SEI_total_data <- function(FarmID,nruns){
  
  library(tidyverse)
  library(patchwork)
  
  source(file = "R/02C_Farm_info.R")
  
  load(paste0("results/IBM_",FarmID,".Rdata"))

  
  First_sample <- Farm_var(FarmID)[[1]]
  time <- as.integer(as.Date("2017-12-31")-First_sample)
  last_visit <- Farm_var(FarmID)[[5]][[6]]
  
  
  # Create dataframes instead of lists-------------------
  IBM_S <- bind_rows(sapply(sim_IBM, "[", 1))
  IBM_E <- bind_rows(sapply(sim_IBM, "[", 2))
  IBM_I <- bind_rows(sapply(sim_IBM, "[", 3))
  
  
  # Calculate mean and confidence intervals -------------
  results_IBM <- bind_cols(IBM_S,IBM_E,IBM_I) %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           Simulation = rep(1:nruns, each = time),
           date = as.Date(First_sample+timestep-1)) %>% 
    group_by(timestep,date, Simulation) %>% 
    summarise(S = sum(S1,S2,S3),
              E = sum(E1,E2,E3),
              I = sum(I1,I2,I3)) %>% ungroup() %>% 
    group_by(timestep,date) %>% 
    summarise(median_S = median(S, na.rm = TRUE),
              median_E = median(E, na.rm = TRUE),
              median_I = median(I, na.rm = TRUE)) %>% 
    pivot_longer(cols = starts_with("m"), names_to = "State", values_to = "Count")
  
  
  return(results_IBM)
  
}



# SEI total plot ----------------------------------------------------------
# C1

C1_data <- SEI_total_data("C1",30)

C1 <- C1_data %>% ggplot(aes(x = date,
                       y = Count,
                       group = State)) +
            geom_line(aes(color = State)) +
            scale_color_manual(values = color_scheme_2[c(5,1,3)],
                               labels = c("Susceptible", "Exposed","Infected")) +
            theme_bw(base_size = 11) +
            labs(title = "Farm C1",
                 x = "",
                 y = "Cattle [#]") +
            scale_x_date(breaks = "4 months", limits = c(min = as.Date("2015-05-04"), 
                                                         max = as.Date("2017-12-31")),
                         date_labels = "%b-%y") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none") + ylim(0,330) 

# C2

C2_data <- SEI_total_data("C2",30)

C2 <- C2_data %>% ggplot(aes(x = date,
                             y = Count,
                             group = State)) +
  geom_line(aes(color = State)) +
  scale_color_manual(values = color_scheme_2[c(5,1,3)],
                     labels = c("Susceptible", "Exposed","Infected")) +
  theme_bw(base_size = 11) +
  labs(title = "Farm C2",
       x = "",
       y = "") +
  scale_x_date(breaks = "4 months", limits = c(min = as.Date("2015-05-05"), 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(0,330) 


# O1

O1_data <- SEI_total_data("O1",30)

O1 <- O1_data %>% ggplot(aes(x = date,
                             y = Count,
                             group = State)) +
  geom_line(aes(color = State)) +
  scale_color_manual(values = color_scheme_2[c(5,1,3)],
                     labels = c("Susceptible", "Exposed","Infected")) +
  theme_bw(base_size = 11) +
  labs(title = "Farm O1",
       x = "Date",
       y = "Cattle [#]") +
  scale_x_date(breaks = "4 months", limits = c(min = as.Date("2015-04-27"), 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(0,330) 


# O2

O2_data <- SEI_total_data("O2",30)

O2 <- O2_data %>% ggplot(aes(x = date,
                             y = Count,
                             group = State)) +
  geom_line(aes(color = State)) +
  scale_color_manual(values = color_scheme_2[c(5,1,3)],
                     labels = c("Susceptible", "Exposed","Infected")) +
  theme_bw(base_size = 11) +
  labs(title = "Farm O2",
       x = "Date",
       y = "") +
  scale_x_date(breaks = "4 months", limits = c(min = as.Date("2015-04-27"), 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(0,330) 

# Legend

legend <- O2_data %>% ggplot(aes(x = date,
                                 y = Count,
                                 group = State)) +
  geom_line(aes(color = State)) +
  scale_color_manual(values = color_scheme_2[c(5,1,3)],
                     labels = c("Susceptible", "Exposed","Infected")) +
  theme_bw(base_size = 11) +
  labs(title = "Farm O2",
       x = "Date",
       y = "") +
  scale_x_date(breaks = "4 months", limits = c(min = as.Date("2015-04-27"), 
                                               max = as.Date("2017-12-31")),
               date_labels = "%b-%y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + ylim(0,330) 

combined_plot <- plot_grid(C1, C2, O1, O2, ncol=2)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot_grid(combined_plot, plot_legend,
          ncol = 1,
          rel_heights = c(18,1))

ggsave(filename = "results/figures/Final_figures/SEI_total.png")




# SEI group plots ---------------------------------------------------------

# C1
plot_SEI("C1",30)
ggsave("results/figures/Final_figures/SEI_groups_C1.png")

# O1
plot_SEI("O1",30)
ggsave("results/figures/Final_figures/SEI_groups_O1.png")






# Validation plot ---------------------------------------------------------

# C1 
plot_validation("C1")
ggsave("results/figures/Final_figures/validation_C1.png")

# C2
plot_validation("C2")
ggsave("results/figures/Final_figures/validation_C2.png")

# O1 
plot_validation("O1")
ggsave("results/figures/Final_figures/validation_O1.png")

# O2 
plot_validation("O2")
ggsave("results/figures/Final_figures/validation_O2.png")





  
