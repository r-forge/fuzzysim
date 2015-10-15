simFromSetOps <- function(size1, size2, intersection, union, total.size = NULL, method = c("Jaccard", "Sorensen", "Simpson", "Baroni"), verbosity = 1) {
  
  method <- match.arg(method, c("Jaccard", "Sorensen", "Simpson", "Baroni"))
  if (verbosity > 0)  message("using ", method, " similarity index")
  
  if (method == "Jaccard") return(intersection / union)

  A <- size1
  B <- size2
  C <- intersection
  
  if (method == "Sorensen") return(2 * C / (A + B))
  else if (method == "Simpson") return(C / min(A, B))

  D <- total.size - union

  if (method == "Baroni") return((sqrt(C * D) + C) / (sqrt(C * D) + A + B - C))
}
