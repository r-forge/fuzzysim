bioThreat <- function(strong_F, weak_F, character = FALSE, ...) {
  stopifnot(length(strong_F) == length(weak_F))
  
  strong_F <- favClass(strong_F, ...)
  weak_F <- favClass(weak_F, ...)
  
  threat <- rep(NA, length(strong_F))
  for (f in 1:length(strong_F)) {
    if (is.na(strong_F[f]) | is.na(weak_F[f])) next
    else if (strong_F[f] == 1 | weak_F[f] == 1) threat[f] <- 0
    else if (strong_F[f] == 3 & weak_F[f] == 3) threat[f] <- 1
    else if (strong_F[f] <= 2 & weak_F[f] == 3) threat[f] <- 2
    else if (strong_F[f] == 2 & weak_F[f] == 2) threat[f] <- 3
    else if (strong_F[f] == 3 & weak_F[f] <= 2) threat[f] <- 4
  }  # end for f
  
  if (character) {
    threat[!is.na(threat) & threat == 0] <- "white"
    threat[!is.na(threat) & threat == 1] <- "green"
    threat[!is.na(threat) & threat == 2] <- "yellow"
    threat[!is.na(threat) & threat == 3] <- "orange"
    threat[!is.na(threat) & threat == 4] <- "red"
  }
  
  else threat <- as.integer(threat)
  
  return(threat)
}
