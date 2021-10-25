#99_Functions
concat_str <- function(x){
  paste(x,collapse = "")
}


min_max <- list(
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE)
)

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


















