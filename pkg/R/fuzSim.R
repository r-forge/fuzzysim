fuzSim <-
function(x, y, method) {
  stopifnot(length(x) == length(y), 
            x >= 0, x <= 1, 
            y >= 0, y <= 1, 
            method %in% c("Jaccard", "Sorensen", "Baroni"))
  A <- sum(x)
  B <- sum(y)
  C <- sum(pmin(x, y))
  if (method == "Jaccard") return(C / (A + B - C))
  else if (method == "Sorensen") return(2 * C / (A + B))
  else if (method == "Baroni") {
    D <- sum(1 - pmax(x, y))
    return((sqrt(C * D) + C) / (sqrt(C * D) + A + B - C))
  }
}
