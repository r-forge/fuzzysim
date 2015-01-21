multConvert <-
function(data, conversion, cols = 1:ncol(data)) {
  for(i in cols) {
    data[ , i] <- conversion(data[ , i])
  }
  return(data)
}
