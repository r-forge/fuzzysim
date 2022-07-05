multConvert <- function(data, conversion, cols = 1:ncol(data)) {
  # version 1.2 (5 Jul 2022)
  
  data <- as.data.frame(data)
  
  for(i in cols) {
    data[ , i] <- conversion(data[ , i])
  }
  
  return(data)
}
