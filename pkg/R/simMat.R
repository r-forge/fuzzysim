simMat <-
  # version 2.0 (23 Apr 2021)
  
function(data, method, diag = TRUE, upper = TRUE, verbosity = 2) {
  if (verbosity > 1)  start.time <- Sys.time()
  stopifnot(data >= 0, data <= 1,
            method %in% c("Jaccard", "Sorensen", "Simpson", "Baroni"))
  n.subjects <- ncol(data)
  sim.mat <- matrix(nrow = n.subjects, ncol = n.subjects,
                    dimnames = list(colnames(data), colnames(data)))
  inds <- triMatInd(sim.mat, lower = TRUE, list = TRUE)
  
  n.pairs <- length(combn(n.subjects, m = 2)) / 2
  
  if (verbosity > 1)  message("Calculating ", n.pairs, " pair-wise similarities...")

  if (verbosity > 0) {
    progbar <- txtProgressBar(min = 0, max = n.pairs, style = 3, char = "-")
  }
  
  # quarter <- round(n.pairs / 4)
  # half <- round(n.pairs / 2)
  # threequarters <- half + quarter
  
  pair <- 0
  for (ind in inds) {
    if (verbosity > 0) {
      setTxtProgressBar(progbar, pair)
    }
    pair <- pair + 1
    row <- rownames(sim.mat)[ind[1]]
    col <- colnames(sim.mat)[ind[2]]
    sim.mat[ind[1], ind[2]] <- fuzSim(x = data[ , row], y = data[ , col], method = method)
    # if(pair == quarter) message ("25% done...")
    # if(pair == half) message ("50% done...")
    # if(pair == threequarters) message ("75% done...")
    # if(pair == n.pairs) message ("Finished!")
  }  # end for ind lower
  
  if (diag) diag(sim.mat) <- 1
  if (upper) {  
    # https://stat.ethz.ch/pipermail/r-help/2008-September/174475.html
    ind <- upper.tri(sim.mat)
    sim.mat[ind] <- t(sim.mat)[ind]
  }
  
  if (verbosity > 1) {
    message ("\nFinished!")
    timer(start.time)
  }
  return(sim.mat)
}
