#99_Functions
concat_str <- function(x){
  paste(x,collapse = "")
}

#Convert negative to 0
conv_neg <- function(x){
  if(x < 0){
    x = 0
  } else{
    x = x
  }
  return(x)
}

#Seasonal function for snail population
#a=amplitude, b=wave number, c=right shift, d=vertical shift

sine <- function(a,b,c,d,x){
  f <- a*sin(b*(x-c))+d
  return(f)
}

# Sampling from the different groups to cohort
cohort <- function(data,distr,nCohort){

  nGroup <- round(distr*nCohort,0)
  
  CohortID <- c()
  
  for(i in 1:length(nGroup)){
     
    CowIDs <- data %>% filter(Group == i) %>% select(CowID) %>% pull() 

    
    CID <- sample(CowIDs,
                 nGroup[i],
                 replace = F)
    
    CohortID <- c(CohortID,CID)
  }
  
  return(CohortID)
}

# Writing sine function to account for ground vs air temperature
sine_ground_vs_air <- function(a_sub,a_add,x){
  #If between 8pm and 8am 
  if(x <= 8 | x> 20){
    #Sine funtion that will be used to add a maximum of 
    #"a_add" degrees to the air temperature
    sine(a=a_add, b=2*pi/24, c=-4, d=0, x=x)
  }
  else{
    #Sine funtion that will be used to subtract a maximum of 
    #"a_sub" degrees to the air temperature
    #Function between 8am and 8pm
    sine(a=a_sub, b=2*pi/24, c=-4, d=0, x=x)
  }
}


















