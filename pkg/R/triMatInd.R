triMatInd <- function(mat, lower = TRUE, list = FALSE) {
  stopifnot(is.matrix(mat), nrow(mat) == ncol(mat))
  n.rows <- nrow(mat)
  seqi <- seq.int(n.rows - 1)
  hi <- rev(abs(sequence(seqi) - n.rows) + 1)
  lo <- rep.int(seqi, rev(seqi))
  if (lower) tri.ind <- cbind(hi, lo, deparse.level = 0)
  else tri.ind <- cbind(lo, hi, deparse.level = 0)
  if (list) tri.ind <- split(tri.ind, row(tri.ind))
  return(tri.ind)
}
