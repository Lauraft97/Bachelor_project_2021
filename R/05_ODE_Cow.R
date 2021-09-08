#05 Structured ODE model

rm(list = ls())

library(tidyverse)
require(deSolve)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")

# Load data ---------------------------------------------------------------
fluke_data_ODE <- read_tsv(file = "data/03_fluke_data_IBM.tsv")


# Selecting one farm ------------------------------------------------------

n_cows <- fluke_data_ODE %>% 
  filter(Farm_ID == 4) %>% 
  group_by(Group_ID) %>% 
  summarise(n = n()) %>% 
  select(n) %>% 
  pull()


SSIR <- function(t,x,p)
{
  ## Unpack state by hand 
  n <- length(x) / 5
  S <- x[1:n]
  E1 <- x[n + (1:n)]
  E2 <- x[2*n + (1:n)]
  E3 <- x[3*n + (1:n)]
  I <- x[4*n + (1:n)]
  
  dS <- numeric(n)
  dE1 <- numeric(n)
  dE2 <- numeric(n)
  dE3 <- numeric(n)
  dI <- numeric(n)
  
  
  with(p,
       {
         for(i in 1:n)
         {
           dS[i] <- - S[i] * beta[i,] %*% I
           dE1[i] <- S[i] * beta[i,] %*% I - gamma[i] * E1[i]
           dE2[i] <- gamma[i] * E1[i] - gamma[i] * E2[i]
           dE3[i] <- gamma[i] * E2[i] - gamma[i] * E3[i]
           dI[i] <-  gamma[i] * E3[i] 
           
         }
         return(list(c(dS,dE1,dE2,dE3,dI)))
       }
  )
}


#Parameters
n <- 4
beta <- diag(c(0.03,0.05,0.02,0.015), c(4,4))
gamma <- rep(4/56,4)

## Initial conditions
I0 <- rep(1,4)
S0 <- n_cows-I0
E1_0 <- 0*I0
E2_0 <- 0*I0
E3_0 <- 0*I0

x0 <- c(S=S0,E1=E1_0,E2=E2_0,E3=E3_0,I=I0)

p <- list(beta=beta,gamma=gamma)

times <- seq(0,356*5,1)

sol <- ode(x0,times,SSIR,p)

head(sol)

tail(sol)



