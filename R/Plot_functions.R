rm(list = ls())


# Plot SEI function -------------------------------------------------------

plot_SEI <- function(FarmID,nruns){
  
  library(tidyverse)
  library(patchwork)
  
  source(file = "R/02C_Farm_info.R")
  
  load(paste0("results/IBM_",FarmID,".Rdata"))
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  
  time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
  last_visit <- Farm_var(FarmID)[[5]][[6]]
  
  
  # Create dataframes instead of lists-------------------
  IBM_S <- bind_rows(sapply(sim_IBM, "[", 1))
  IBM_E <- bind_rows(sapply(sim_IBM, "[", 2))
  IBM_I <- bind_rows(sapply(sim_IBM, "[", 3))
  
  
  # Calculate mean and confidence intervals -------------
  results_IBM_S <- IBM_S %>% 
    mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
    pivot_longer(cols = starts_with("S"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group) %>%
    summarise(mean = mean(Count, na.rm = TRUE),
              sd = sd(Count, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_IBM_E <- IBM_E %>% 
    mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
    pivot_longer(cols = starts_with("E"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group) %>%
    summarise(mean = mean(Count, na.rm = TRUE),
              sd = sd(Count, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_IBM_I <- IBM_I %>% 
    mutate(timestep = rep(seq(1,time,1),nruns)) %>% 
    pivot_longer(cols = starts_with("I"), names_to = "Group", values_to = "Count") %>%
    group_by(timestep, Group) %>%
    summarise(mean = mean(Count, na.rm = TRUE),
              sd = sd(Count, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  
  #Plot -------------------------------------------
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
    geom_vline(xintercept = last_visit,
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
    geom_vline(xintercept = last_visit,
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
    geom_vline(xintercept = last_visit,
               color = "gray60",
               linetype = "dashed") +
    theme(legend.position = "none")
  
  p <- S/E/I + plot_annotation(title = paste("Farm", FarmID, sep = " "))
  
  
  # Return from function ----------------

  return(p)
}


# Plot ODE function -------------------------------------------------------

plot_ODE <- function(FarmID,ODEVar){
  library(tidyverse)
  library(patchwork)
  
  source(file = "R/02C_Farm_info.R")
  
  load(paste0("results/ODE_",FarmID,".Rdata"))
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  color_df <- as.data.frame(cbind(var = c("Non", "G1", "G3", "C1", "C2", "O1", "G2", "O2"),
                                 color_scheme))
  
  Farm_col <- color_df %>% filter(var == FarmID) %>% select(color_scheme) %>% pull()
  
  
  time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
  City <- Farm_var(FarmID)[[3]]
  last_visit <- Farm_var(FarmID)[[5]][[6]]
  
  # Load data -----------------------
  
  Egg <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 1)))   
  Exposed_1 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 2)))   
  Exposed_2 <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 3)))   
  Infected <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 4)))
  Susceptible_snail <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 5)))
  Snail_population <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 6)))
  Metacercariae <- as.data.frame(do.call(cbind, sapply(sim_ODE, "[", 7)))  
  
  
  
  # Calculate mean and CI -----------------------
  results_ODE <- eval(parse(text = ODEVar)) %>%
    rowwise() %>% 
    mutate(mean = mean(c_across(starts_with("V"))),
           sd = sd(c_across(starts_with("V"))),
           n = length(c_across(starts_with("V"))),
           se = sd / sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
           lower.ci = if_else(lower.ci < 0,0,lower.ci)) 
  
  # Plot results -----------------------
  
  p <- results_ODE %>% ggplot(aes(x = 1:time,
                                y = mean,
                             color = "Mean")) +
    geom_line() +
    scale_color_manual(values=Farm_col,
                       labels = c("Mean")) +
    theme_bw(base_size = 8) +
    labs(x = "Time [Days]",
         y = "Cattle [#]",
         title = ODEVar,
         subtitle = paste("Location:", City, sep = " " )) +
    theme(legend.title = element_blank())
  
  
  return(p)
}

