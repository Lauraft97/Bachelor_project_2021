#Probabilities for infection

metac <- M

x <- 10^4

cow_prop_mean <- seq(1,length(metac))

for(i in 1:length(metac)){
  
  M_1 <- metac[i]
  grazing <- runif(300,0,1)
  prop <- 1-exp(-grazing*M_1/x)
  
  cow_prop_mean[i] <- mean(prop)
  
}


visits <- seq(1,6)

visits[1] <- mean(cow_prop_mean[1:95])
visits[2] <- mean(cow_prop_mean[96:195])
visits[3] <- mean(cow_prop_mean[196:276])
visits[4] <- mean(cow_prop_mean[277:345])
visits[5] <- mean(cow_prop_mean[346:474])
visits[6] <- mean(cow_prop_mean[475:620])


O1 <- I_pr_cow_pr_day[3,-1]*300
C1 <- I_pr_cow_pr_day[1,-1]*300
O2 <- I_pr_cow_pr_day[4,-1]*300
C2 <- I_pr_cow_pr_day[2,-1]*300



plot(cow_prop_mean) 

plot(visits)
points(O1,col = 2)
points(C1,col = 3)
points(O2,col = 4)
points(C2,col = 5)
abline(h=O1,col = "red")
abline(h=C1,col = "green")
abline(h=O2,col = "blue")


