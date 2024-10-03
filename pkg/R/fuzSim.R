fuzSim <- function(x, y, method, na.rm = TRUE) {

  # version 2.0 (3 Oct 2024)

  if (inherits(x, "SpatRaster"))
    x <- terra::values(x, mat = FALSE, dataframe = FALSE)
  if (inherits(y, "SpatRaster"))
    y <- terra::values(y, mat = FALSE, dataframe = FALSE)

  x <- unlist(x)
  y <- unlist(y)

  method <- match.arg(method, c("Jaccard", "Sorensen", "Simpson", "Baroni"))
  dab.methods <- c("Baroni")

  if (na.rm) {
    data <- cbind(x, y)
    data <- na.omit(data)
    x <- data[ , 1]
    y <- data[ , 2]
  }

  stopifnot (length(x) == length(y),
             #x >= 0 & x <= 1,
             #y >= 0 & y <= 1
             min(c(x, y, na.rm = TRUE)) >= 0,
             max(c(x, y, na.rm = TRUE)) <= 1
             )

  A <- sum(x)
  B <- sum(y)
  C <- sum(pmin(x, y))
  if (method %in% dab.methods) D <- sum(1 - pmax(x, y))

  if (method == "Jaccard") return(C / (A + B - C))
  else if (method == "Sorensen") return(2 * C / (A + B))
  else if (method == "Simpson") return(C / min(A, B))
  else if (method == "Baroni") return((sqrt(C * D) + C) / (sqrt(C * D) + A + B - C))
}
