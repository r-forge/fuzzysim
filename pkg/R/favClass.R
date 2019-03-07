favClass <- function(fav, breaks = c(0.2, 0.8), character = FALSE) {
  
  stopifnot(
    is.numeric(fav),
    is.numeric(breaks),
    length(breaks) == 2,
    fav >= 0,
    fav <= 1,
    breaks >= 0,
    breaks <= 1
  )
  
  fclass <- rep(NA, length(fav))
  b1 <- breaks[1]
  b2 <- breaks[2]
  for (f in 1:length(fav)) {
    if (is.na(fav[f])) next
    if (fav[f] < b1) fclass[f] <- 1
    if (fav[f] >= b1 & fav[f] < b2) fclass[f] <- 2
    if (fav[f] >= b2) fclass[f] <- 3
  }
  
  if (character) {
    fclass[!is.na(fclass) & fclass == 1] <- "low"
    fclass[!is.na(fclass) & fclass == 2] <- "intermediate"
    fclass[!is.na(fclass) & fclass == 3] <- "high"
  }
  
  else fclass <- as.integer(fclass)
  
  return(fclass)
}