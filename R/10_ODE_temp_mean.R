#ODE_rates_mean_temp

# Defining the maximum rates 
lambda_ES_max <- 0.0000005 #Transmission rate egg to snail
mu_Egg_max <- 0.65 # Death rate eggs (become non-infectious)
delta_snail_max <- 1.5 #Daily snail population "scaling factor"
gamma_S_max <- 2 #Excretion of metacercarria from snail
mu_S_max <- 0.05 #Death rate of infected snails / recovery rate
mu_M_max <- 0.15 # Death rate of metacercaria


Rates <- function(date){
  
  #Filtering data for given day  
  DD <- daily_weather %>% filter(Date == date,location == City) %>% 
    select(mean_ground_temp_ten) %>% 
    pull()
  
  last10Days <- date - 0:9
  
  rain <- daily_weather %>% filter(Date %in% last10Days,location == City) %>% 
    summarise(rain = sum(rain)) %>% pull()
  
  rate_exp <-0.2
  
  if(rain < 1){
    
    lambda_ES <- 0
    mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max*1.5
    mu_S <- (1-pexp(q = DD,rate = rate_exp))*mu_S_max*1.5
    mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max*1.5
  }
  
  if(rain >= 1 | rain < 10){
    
    lambda_ES <- pexp(q = DD,rate = rate_exp)*lambda_ES_max
    mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max*1.25
    mu_S <- (1-pexp(q = DD,rate = rate_exp))*mu_S_max*1.25
    mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max*1.25
    
  }
  
  if(rain >= 10){
    
    lambda_ES <- pexp(q = DD,rate = rate_exp)*lambda_ES_max
    mu_Egg <- (1-pexp(q = DD,rate = rate_exp))*mu_Egg_max
    mu_S <- (1-pexp(q = DD,rate = rate_exp))*mu_S_max
    mu_M <- (1-pexp(q = DD,rate = rate_exp))*mu_M_max
    
  }
  

  delta_snail <- pexp(q = DD,rate = rate_exp)*delta_snail_max

  

  
  
  Rates <- c(lambda_ES,mu_Egg,delta_snail,mu_M)
  return(Rates) 
  
}

# rain_t <- rep(0,365*2)
# rain_f <- rep(0,365*2)
# 
# for(i in c(1:(365*2))){
# 
#   date <- First_sample + i
# 
#   last10Days <- date - 0:9
# 
#   rain_t[i] <- daily_weather %>% filter(Date %in% last10Days,location == "Toender") %>%
#     summarise(rain = sum(rain)) %>% pull()
# 
#   rain_f[i] <- daily_weather %>% filter(Date %in% last10Days,location == "Frederikssund") %>%
#     summarise(rain = sum(rain)) %>% pull()
# }
# 
# rain <- tibble(rain_t, rain_f)
# 
# rain <- rain %>% mutate(diff = rain_t - rain_f)
# 
# rain %>% ggplot(aes(x = 1:(365*2))) +
#          geom_line(aes(y = rain_t)) +
#          geom_line(aes(y = rain_f, col = "red")) +
#          geom_hline(yintercept = 15)
# 
# rain %>% filter(rain_f > 1 & rain_t < 1)

#
# rain %>% ggplot(aes(x = 1:(365*2))) +
#   geom_line(aes(y = diff))
#
# daily_weather %>% filter(Date >= First_sample & Date <= First_sample + 730) %>% ggplot(aes(x = Date,
#                                      y = mean_ground_temp_ten,
#                                      col = location)) +
#   geom_line()
#
#
#
# plot(seq(0,13,0.5),pexp(q = seq(0,13,0.5),rate = 0.2))
