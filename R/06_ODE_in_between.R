
rm(list=ls())

require(deSolve)


SIR <- function(t,x,p)
{
  with(as.list(c(x,p)),
       {
         
        dEggs <- Eggs - mu_Egg * Egg - lambda_ES * Egg * S_S
        dE1_S <- lambda_ES * Egg * S_S - alpha * E1_S  
        dE2_S <- alpha * E1_S - alpha * E2_S  
        dI_S <-  alpha * E2_S - mu_S * I_S
        dR_S <-  mu_S * I_S
        dM <- gamma_S * I_S - mu_M * M
        
         
         return(list(c(dEggs,dE1_S,dE2_S,dI_S,dR_S,dM)))
       }
  )
}


# Parameters --------------------------------------------------------------
Eggs <- 30*1000*5*6
mu_Egg <- 0.1
lambda_ES <- 2 * 10^(-10)
S_S <- 10^4
alpha <- 2/(6*7)
gamma_S <- 2
mu_S <- 0.05
mu_M <- 0.05

# Initial values

Eggs0 <- 10
E1_S0 <- 0
E2_S0 <- 0
I_S0 <- 0
R_S0 <- 0
M0 <- 1000
 
  
p <- list(Eggs = Eggs,
          mu_Egg = mu_Egg,
          lamdba_ES = lambda_ES,
          S_S = S_S,
          alpha = alpha, 
          gamma_S = gamma_S,
          mu_S = mu_S,
          mu_M = mu_M)

x0 <- c(Egg = Eggs0,
        E1_S = E1_S0,
        E2_S = E2_S0,
        I_S = I_S0,
        R_S = R_S0,
        M = M0)

times <- seq(1,200,1)

sol <- ode(x0,times,SIR,p)


head(sol)
tail(sol)

sol[,4]

matplot(times,sol[,-c(1:2,6:7)],type="l",xlab="Time",ylab="")
