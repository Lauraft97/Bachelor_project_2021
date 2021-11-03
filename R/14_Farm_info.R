# 14_Farm variables


# fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")
# 
# 
# fluke_data %>% filter(Farm == "O2") %>% distinct(Date) %>% arrange(Date) %>% pull()
# 
# fluke_data %>% filter(Farm == "O2") %>% distinct(UniqueID)
# 
# fluke_diag %>% group_by(Visit) %>%  filter(Farm == "O2", Diag == T) %>% distinct(UniqueID) %>% 
# summarise(n = n())

Farm_var <- function(Farm_name){
  
  if(Farm_name == "C1"){
    # Data 
    First_sample = as.Date("2015-05-04")
    Visits = as.Date(c("2015-08-06","2015-11-17", "2016-02-05",
                       "2016-04-11", "2016-08-22", "2017-01-19" ))
    Location = "Frederikssund"
    Cohort_size = 49
    cDistr = c(1/4,1/4,2/4)
    nCow = 312
    nE0 = 8
    egg_mu_distr = 1.2095
  }
  
  if(Farm_name == "C2"){
    # Data 
    First_sample = as.Date("2015-05-05")
    Visits = as.Date(c("2015-08-03", "2015-10-22", "2016-01-26",
                       "2016-04-05", "2016-08-29", "2017-01-10"))
    Location = "Toender"
    Cohort_size = 56
    cDistr = c(1/4,1/4,2/4)
    nCow = 331
    nE0 = 15
    egg_mu_distr = 2.0103
  }
  
  if(Farm_name == "O1"){
    # Data 
    First_sample = as.Date("2015-04-27")
    Visits = as.Date(c("2015-07-27", "2015-11-11", "2016-02-02",
                       "2016-04-14", "2016-08-15", "2017-01-16" ))
    Location = "Toender"
    Cohort_size = 60
    cDistr = c(1/4,1/4,2/4)
    nCow = 425
    nE0 = 20
    egg_mu_distr = 5.4549
  }
  
  if(Farm_name == "O2"){
    # Data 
    First_sample = as.Date("2015-04-27")
    Visits = as.Date(c("2015-08-03", "2015-11-11", "2016-01-26",
                       "2016-04-05", "2016-08-29", "2017-01-10" ))
    Location = "Toender"
    Cohort_size = 64
    cDistr = c(1/4,1/4,2/4)
    nCow = 285
    nE0 = 20
    egg_mu_distr = 3.3353
  }
  
  # Visits in k count
  visit_k <- as.numeric(Visits - First_sample)
  
  Farm_info <- list(First_sample, Location, Cohort_size, nCow, visit_k, cDistr, nE0,egg_mu_distr)
  
  return(Farm_info)
  
  
}
