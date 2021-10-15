#ODE_rates_mean_temp

# Defining the maximum rates 
lambda_ES_max <- 0.0000005 #Transmission rate egg to snail
mu_Egg_max <- 0.65 # Death rate eggs (become non-infectious)
delta_snail_max <- 1.5 #Daily snail population "scaling factor"
gamma_S_max <- 2 #Excretion of metacercarria from snail
mu_S_max <- 0.05 #Death rate of infected snails / recvoery rate
mu_M_max <- 0.15 # Death rate of metacercaria




Rates <- function(date){
  
  
  #Filtering data for given day  
  DD <- daily_weather %>% filter(Date == date,location == "Toender") %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
  
  rate_exp <-0.2
  
  lambda_ES <- pexp(q = DD,rate = rate_exp)*lambda_ES_max
  mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max
  delta_snail <- pexp(q = DD,rate = rate_exp)*delta_snail_max
  mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max
  
  Rates <- c(lambda_ES,mu_Egg,delta_snail,mu_M)
  return(Rates) 
  
}





plot(seq(0,13,0.5),pexp(q = seq(0,13,0.5),rate = 0.2))
