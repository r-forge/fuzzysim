transpose <- function(data, sp.cols = 1:ncol(data), reg.names = NULL) {
  transp <- as.data.frame(t(data[ , sp.cols]))
  colnames(transp) <- data[ , reg.names]
  return(transp)
}
