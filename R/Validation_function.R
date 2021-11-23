plot_validation <- function(FarmID){
  
  FarmID = "C1"

  library(tidyverse)
  
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
    scale_shape_manual(values=c(rep(16,10),4)) +
    scale_color_manual(values = c(rep(Farm_col,10),"black")) +
    scale_size_manual(values=c(rep(1,10),3)) +
    theme_bw() + 
    theme(legend.position = "none") +
    labs(title = paste0("Farm ", FarmID, " simulations and farm data")) 
 
 legend_1 <- plot_data %>% filter(Simulation == 1) %>% 
             ggplot(aes(Visit,
                        Count,
                        group = Simulation)) +
   geom_point(aes(color = Simulation)) +
   geom_line(aes(color = Simulation)) +
   scale_color_manual(values = Farm_col,
                      label = "Simulations",
                      name = NULL) +
   theme_bw()
 
 
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
             theme_bw()
 
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

  
  
  

