splist2presabs <- function(data, sites.col, sp.col, keep.n = FALSE) {
  
  # version 1.2 (5 Jul 2022)

  data <- as.data.frame(data)
  
  stopifnot(
    length(sites.col) == 1,
    length(sp.col) == 1,
    sites.col != sp.col,
    sites.col %in% 1:ncol(data) | sites.col %in% colnames(data),
    sp.col %in% 1:ncol(data) | sp.col %in% colnames(data),
    is.logical(keep.n)
  )
  presabs <- table(data[ , c(sites.col, sp.col)])
  presabs <- as.data.frame(unclass(presabs))
  if (!keep.n)  presabs[presabs > 1] <- 1
  presabs <- data.frame(row.names(presabs), presabs)
  names(presabs)[1] <- names(subset(data, select = sites.col))
  rownames(presabs) <- NULL
  return(presabs)
}
