transpose <- function(data, sp.cols = 1:ncol(data), reg.names = NULL) {
  # version 1.2 (5 Jul 2022)
  
  data <- as.data.frame(data)
  transp <- as.data.frame(t(data[ , sp.cols]))
  colnames(transp) <- data[ , reg.names]
  return(transp)
}
