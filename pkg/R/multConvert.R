multConvert <- function(data, conversion, cols = 1:ncol(data)) {
  # version 1.3 (6 Sep 2025)

  if (!inherits(data, "SpatRaster")) {
    data <- as.data.frame(data)  # for matrices, tibbles, etc.
    # data[ , cols] <- conversion(data[ , cols])  # Error: 'list' object cannot be coerced to type 'integer'
    data[ , cols] <- sapply(data[ , cols], conversion)
  }

  else data[[cols]] <- conversion(data[[cols]])

  return(data)
}
