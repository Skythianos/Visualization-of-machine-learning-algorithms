normalize <- function(x) {
  min <- min(x)
  max <- max(x)
  
  (x-min)/(max - min)
}