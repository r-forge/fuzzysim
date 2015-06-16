simFromSetOps <- function(size1, size2, intersection, union, total.size = NULL, method = c("Jaccard", "Sorensen", "Simpson", "Baroni")) {
  
  method <- match.arg(method, c("Jaccard", "Sorensen", "Simpson", "Baroni"))
  dab.methods <- c("Baroni")
  if (method %in% dab.methods & is.null(total.size)) stop (method, " method requires specifying 'total.size'.")
  
  A <- size1
  B <- size2
  C <- intersection
  D <- total.size - union
  
  if (method == "Jaccard") sim <- C / union
  else if (method == "Sorensen") sim <- 2 * C / (A + B)
  else if (method == "Simpson") sim <- C / min(A, B)
  else if (method == "Baroni") sim <- (sqrt(C * D) + C) / (sqrt(C * D) + A + B - C)
  return(sim)
}