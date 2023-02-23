CI <- function(x, ci=0.95){
  
  interval <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  
  return(c(mean(x)-interval, mean(x)+interval))
  
}