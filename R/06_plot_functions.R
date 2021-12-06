
# Plot SEI function -------------------------------------------------------
plot_SEI <- function(FarmID,nruns){
  
  library(tidyverse)
  library(patchwork)
  
  source(file = "R/02C_Farm_info.R")
  
  load(paste0("results/IBM_",FarmID,".Rdata"))
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  First_sample <- Farm_var(FarmID)[[1]]
  time <- as.integer(as.Date("2017-12-31")-First_sample)
  last_visit <- First_sample + Farm_var(FarmID)[[5]][[6]]
  
  
  
  # Create dataframes instead of lists-------------------
  IBM_S <- bind_rows(sapply(sim_IBM, "[", 1))
  IBM_E <- bind_rows(sapply(sim_IBM, "[", 2))
  IBM_I <- bind_rows(sapply(sim_IBM, "[", 3))
  
  
  # Calculate mean and confidence intervals -------------
  results_IBM_S <- IBM_S %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("S"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group,date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75") 
  
  results_IBM_E <- IBM_E %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("E"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group, date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75")
  
  results_IBM_I <- IBM_I %>% 
    mutate(timestep = rep(seq(1,time,1),nruns),
           date = as.Date(First_sample+timestep-1)) %>% 
    pivot_longer(cols = starts_with("I"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group, date) %>%
    summarise(x = quantile(Count,c(0.25,0.5,0.75)),
              q = c(0.25,0.5,0.75)) %>% 
    pivot_wider(names_from = "q", values_from = "x", id_cols = c(timestep, Group, date)) %>% 
    rename("Q1" = "0.25", "median" = "0.5", "Q3" = "0.75")
  
  
  #Plot -------------------------------------------
  S <- results_IBM_S %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Susceptible") +
    theme(axis.text.x=element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0), "cm"))+ 
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + ylim(0,170)
  
  
  E <- results_IBM_E %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Exposed") +
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + 
    theme(axis.text.x=element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"))
  
  
  I <- results_IBM_I %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 11,
             base_family = "Lucida Bright") +
    labs(x = "Date",
         y = "Cattle [#]",
         title = "Infected") +
    scale_x_date(breaks = "2 months", limits = c(min = First_sample, 
                                                 max = as.Date("2017-12-31")),
                 date_labels = "%b-%y") + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin=unit(c(0,0,0,0), "cm"))
  
  p <- S/E/I + plot_annotation(title = paste("Farm", FarmID, sep = " "),
                               subtitle = "IBM - Median of simulations") & 
    theme(plot.title = element_text(family = "Lucida Bright"),
          plot.subtitle = element_text(family = "Lucida Bright"))
  
  
  # Return from function ----------------
  
  return(p)
}


# Plot validation ---------------------------------------------------------
plot_validation <- function(FarmID){
  
  library(tidyverse)
  library(cowplot)
  
  # Import functions 
  source(file ="R/99_functions.R")
  source(file = "R/02C_Farm_info.R")
  
  
  # Preparing farm data -------------------------------------------------
  fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")
  
  fluke_diag <- fluke_data %>% rowwise() %>% 
    mutate(Diag = sum(as.numeric(dEPG),
                      as.numeric(dSerum),
                      as.numeric(dCopro),
                      na.rm = TRUE),
           Diag = if_else(Diag > 0, TRUE, FALSE))
  
  farm_data <- fluke_diag %>% filter(Farm == FarmID, Diag == T) %>% group_by(Visit) %>% 
    summarise(Count = n()) %>% slice(-1) %>% mutate(Simulation = "Farm data")
  
  # Simulation data ---------------------------------------
  
  load(paste0("results/validation_",FarmID,".Rdata"))
  
  visit_days_n <- Farm_var(FarmID)[[5]]
  
  validation <- bind_rows(sim_validation, .id = "Simulation")
  
  sim_data <- validation %>% filter(I_period > 0) %>% 
    group_by(Visit_day_no,Simulation) %>% summarise(Count = n()) %>% 
    ungroup() %>% mutate(Visit = case_when(Visit_day_no == visit_days_n[1] ~ "B",
                                           Visit_day_no == visit_days_n[2] ~ "C",
                                           Visit_day_no == visit_days_n[3] ~ "D",
                                           Visit_day_no == visit_days_n[4] ~ "E",
                                           Visit_day_no == visit_days_n[5] ~ "F",
                                           Visit_day_no == visit_days_n[6] ~ "G"))
  
  #Plot ------------------------------------------------------------------
  
  plot_data <- bind_rows(farm_data,sim_data)
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  color_df <- as.data.frame(cbind(var = c("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2"),
                                  color_scheme))
  Farm_col <- color_df %>% filter(var == FarmID) %>% select(color_scheme) %>% pull()
  
  
  
  full <- plot_data %>%  ggplot(aes(x = Visit,
                                    y = Count,
                                    group = Simulation)) + 
    geom_point(aes(shape = Simulation, 
                   color = Simulation,
                   size = Simulation)) +
    geom_line(aes(color = Simulation)) + 
    scale_shape_manual(values=c(rep(16,30),4)) +
    scale_color_manual(values = c(rep(Farm_col,30),"black")) +
    scale_size_manual(values=c(rep(1,30),3)) +
    theme_bw(base_family = "Lucida Bright",
             base_size = 12) + 
    theme(legend.position = "none") +
    labs(title = paste0("Farm ", FarmID),
         subtitle = "Cohort validation",
         y = "Cattle [#]") 
  
  legend_1 <- plot_data %>% filter(Simulation == 1) %>% 
    ggplot(aes(Visit,
               Count,
               group = Simulation)) +
    geom_point(aes(color = Simulation)) +
    geom_line(aes(color = Simulation)) +
    scale_color_manual(values = Farm_col,
                       label = "Simulations",
                       name = NULL) +
    theme_bw(base_size = 12)
  
  
  legend_2 <- plot_data %>% filter(Simulation == "Farm data") %>% 
    ggplot(aes(Visit,
               Count,
               group = Simulation)) +
    geom_point(aes(color = Simulation),
               shape = 4,
               size = 3) +
    geom_line(aes(color = Simulation)) +
    scale_color_manual(values = "black",
                       name = NULL) +
    theme_bw(base_size = 12)
  
  p <- plot_grid(full,
                 plot_grid(get_legend(legend_1),
                           NULL,
                           get_legend(legend_2),
                           nrow = 3,
                           align = "v",
                           rel_heights = c(1,-0.9,1)),
                 ncol = 2,
                 rel_widths = c(7,1),
                 align = "h")
  
  return(p)    
  
}  











