plot_validation_group <- function(FarmID,GroupNo){
  
  library(tidyverse)
  
  # Import functions 
  source(file ="R/99_functions.R")
  source(file = "R/02C_Farm_info.R")
  
  color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
  
  # Preparing farm data 
  fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")
  fluke_data <- fluke_data %>% mutate(data_Group = if_else(str_detect(Group,"s$"),"Cow", Group))
                                      
  #Parameters
  year <- 365
  month10 <- 304
  
  time <- as.integer(as.Date("2017-12-31")-Farm_var(FarmID)[[1]])
  First_sample <- Farm_var(FarmID)[[1]]
  visit_days_n <- Farm_var(FarmID)[[5]]
  
  
  # The age of each cow in days at each visit
  fluke_data <- fluke_data %>% mutate(Cow_Age = as.numeric(Date - DOB))
  
  # Subsetting all the cows which at some point have a positive test
  # Divides data sets into each farm
  
  fluke_diag <- fluke_data %>% rowwise() %>% 
    mutate(Diag = sum(as.numeric(dEPG),
                      as.numeric(dSerum),
                      as.numeric(dCopro),
                      na.rm = TRUE),
           Diag = if_else(Diag > 0, TRUE, FALSE)) %>% 
    filter(Diag == TRUE)
  
  
  #Finding the age of the cow when it was first infected (when it was the youngest)
  
  fluke_infected <- fluke_diag %>% group_by(UniqueID) %>% 
    mutate(age_inf = min(Cow_Age)) %>% 
    filter(Cow_Age == age_inf & Cow_Age > 0) %>% 
    mutate(Group = case_when(Cow_Age <= month10 ~ 1,
                                   Cow_Age > month10 & First_sample - DOB <= 2*year ~ 2,
                                   Cow_Age > 2*year ~ 3))
  
  # Summary data frame of infected in the groups at in visit on each farm
  inf_count <- fluke_infected %>% filter(Farm == FarmID) %>%
    group_by(Farm, Visit, Group) %>% summarise(Count = n()) 
  
  

  # Preparing simulation data
  
    load(paste0("results/validation_",FarmID,".Rdata"))
    validation <- bind_rows(sim_validation, .id = "Simulation")
    
    results_validation <- validation %>% filter(I_period > 0) %>% 
      group_by(Visit_day_no,Group,Simulation) %>% summarise(Count = n()) %>% 
      ungroup()
    
    
    sim_group <- results_validation %>% mutate(Visit = case_when(Visit_day_no == visit_days_n[1] ~ "B",
                                                                 Visit_day_no == visit_days_n[2] ~ "C",
                                                                 Visit_day_no == visit_days_n[3] ~ "D",
                                                                 Visit_day_no == visit_days_n[4] ~ "E",
                                                                 Visit_day_no == visit_days_n[5] ~ "F",
                                                                 Visit_day_no == visit_days_n[6] ~ "G")) %>% 
                                                 filter(Group == GroupNo)
    
    
    inf_count <- inf_count %>% mutate(Simulation = "Data") %>%  
                  filter(Group == GroupNo, Farm == FarmID, Visit != "A") 
    
    
    plot_data <- bind_rows(sim_group,inf_count)
    
    
    
    p <- plot_data %>%  ggplot(aes(x = Visit,
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
    
    
  return(p)
}


plot_validation_group("C1",3)


rm(list=ls())


FarmID = "C1"
GroupNo = 2
