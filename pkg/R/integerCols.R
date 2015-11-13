integerCols <- function(data) {
  is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
    abs(x - round(x)) < tol
  }  # from ?is.integer examples
  all.cols <- 1:ncol(data)
  integer.cols <- rep(0, ncol(data))
  for (i in all.cols) {
    x <- na.omit(data[ , i])
    if(!is.numeric(x)) next
    if(!all(is.finite(x))) next
    if(min(is.wholenumber(x) == 1)) integer.cols[i] <- 1
  }
  multConvert(data, conversion = as.integer, cols = all.cols[integer.cols == 1])
}
