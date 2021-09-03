#99_Functions
concat_str <- function(x){
  paste(x,collapse = "")
}


min_max <- list(
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE)
)
