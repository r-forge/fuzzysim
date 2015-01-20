fuzSim <-
function(x, y, method) {
  stopifnot (length(x) == length(y), x >= 0 & x <= 1, y >= 0 & y <= 1)
  method <- match.arg(method, c("Jaccard", "Sorensen", "Baroni", "Simpson", "matching"))
  dab.methods <- c("Baroni", "matching")
  
  A <- sum(x)
  B <- sum(y)
  C <- sum(pmin(x, y))
  if (method %in% dab.methods) D <- sum(1 - pmax(x, y))

  if (method == "Jaccard") return(C / (A + B - C))
  else if (method == "Sorensen") return(2 * C / (A + B))
  else if (method == "Baroni") return((sqrt(C * D) + C) / (sqrt(C * D) + A + B - C))
  else if (method == "Simpson") return(C / min(A, B))
  else if (method == "matching") return((C + D) / (A + B + D))
}
