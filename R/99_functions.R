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

# Logistic growth
log_growth <- function(max,halfway,rate,k){
  f <- max/(1+exp(-rate*(k-halfway)))
  return(f)
}

#Seasonal function for snail population
#a=amplitude, b=wave number, c=right shift, d=vertical shift

sine <- function(a,b,c,d,x){
  f <- a*sin(b*(x-c))+d
  return(f)
}

