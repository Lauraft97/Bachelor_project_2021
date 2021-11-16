#Vectors to fill our in ODE
Eggs <- c(rep(0,time))
E1_S <- c(rep(0,time))  
E2_S <- c(rep(0,time))
I_S <-  c(rep(0,time))
S_S <- c(rep(0,time))
M <- c(rep(0,time))
Snail_pop <- c(rep(0,time))
Snail_prev <- c(rep(0,time))

# Data frames to store results for each group
S_Cow <- tibble(S1 = rep(0,time),
                S2 = rep(0,time),
                S3 = rep(0,time))

E_Cow <- tibble(E1 = rep(0,time),
                E2 = rep(0,time),
                E3 = rep(0,time))

I_Cow <- tibble(I1 = rep(0,time),
                I2 = rep(0,time),
                I3 = rep(0,time))

validation <- tibble(CowID = 0, 
                     DOB = NA,
                     I_period = 0,
                     Visit_day_no = 0)



Egg_new <- c(rep(0,time))
Births <- c(rep(0,time))
Pop <- c(rep(0,time))