bioThreat <- function(strong_F, weak_F, character = FALSE, ...) {
  stopifnot(length(strong_F) == length(weak_F))
  
  strong_F <- favClass(strong_F, ...)
  weak_F <- favClass(weak_F, ...)
  
  threat <- rep(NA, length(strong_F))
  threat[strong_F == 3 & weak_F <= 2] <- 4
  threat[strong_F == 2 & weak_F == 2] <- 3
  threat[strong_F <= 2 & weak_F == 3] <- 2
  threat[strong_F == 3 & weak_F == 3] <- 1
  threat[strong_F == 1 | weak_F == 1] <- 0

  if (character) {
    threat[!is.na(threat) & threat == 4] <- "red"
    threat[!is.na(threat) & threat == 3] <- "orange"
    threat[!is.na(threat) & threat == 2] <- "yellow"
    threat[!is.na(threat) & threat == 1] <- "green"
    threat[!is.na(threat) & threat == 0] <- "white"
  }
  else threat <- as.integer(threat)
  
  return(threat)
}
