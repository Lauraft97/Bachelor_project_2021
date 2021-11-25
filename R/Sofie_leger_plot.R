# Plot SEI function -------------------------------------------------------

  FarmID = "O1"
  nruns = 10
  
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
    theme_bw(base_size = 12,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Susceptible") +
    theme(axis.text.x=element_blank())+ 
    geom_vline(xintercept = last_visit,
               color = "gray60",
               linetype = "dashed") +
    theme(legend.position = "none")
  
  
  E <- results_IBM_E %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 12,
             base_family = "Lucida Bright") +
    labs(x = "",
         y = "Cattle [#]",
         title = "Exposed") +
    geom_vline(xintercept = last_visit,
               color = "gray60",
               linetype = "dashed") +
    theme(axis.text.x=element_blank())
  
  
  I <- results_IBM_I %>% ggplot(aes(x = date,
                                    y = median,
                                    color = Group)) +
    geom_rect(aes(xmin = First_sample,
                  xmax = as.Date("2015-05-31"),
                  ymin = -Inf, ymax = Inf),fill = "gray90", 
              color = NA, alpha =0.1)+
    geom_line() + 
    scale_color_manual(values=c(color_scheme[2], 
                                color_scheme[7], 
                                color_scheme[3]),
                       labels = c("Calf", "Heifer", "Cow")) +
    theme_bw(base_size = 12,
             base_family = "Lucida Bright") +
    labs(x = "Date",
         y = "Cattle [#]",
         title = "Infected") + 
    geom_vline(xintercept = last_visit,
               color = "gray60",
               linetype = "dashed") +
    theme(legend.position = "none")
  
   S/E/I + plot_annotation(title = paste("Farm", FarmID, sep = " ")) & 
    theme(plot.title = element_text(family = "Lucida Bright"))
  
  
  # Return from function ---------------