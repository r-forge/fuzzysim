simMat <-
function(data, method, diag = TRUE, upper = TRUE) {
  start.time <- Sys.time()
  stopifnot(data >= 0, data <= 1,
            method %in% c("Jaccard", "Sorensen", "Simpson", "Baroni"))
  n.subjects <- ncol(data)
  sim.mat <- matrix(nrow = n.subjects, ncol = n.subjects,
                    dimnames = list(colnames(data), colnames(data)))
  inds <- triMatInd(sim.mat, lower = TRUE, list = TRUE)
  
  n.pairs <- length(combn(n.subjects, m = 2)) / 2
  quarter <- round(n.pairs / 4)
  half <- round(n.pairs / 2)
  threequarters <- half + quarter
  message("Calculating ", n.pairs, " pair-wise similarities...")
  pair <- 0
  for (ind in inds) {
    pair <- pair + 1
    row <- rownames(sim.mat)[ind[1]]
    col <- colnames(sim.mat)[ind[2]]
    sim.mat[ind[1], ind[2]] <- fuzSim(x = data[ , row], y = data[ , col], method = method)
    if(pair == quarter) message ("25% done...")
    if(pair == half) message ("50% done...")
    if(pair == threequarters) message ("75% done...")
    if(pair == n.pairs) message ("Finished!")
  }  # end for ind lower
  
  if (diag) diag(sim.mat) <- 1
  if (upper) {  
    # https://stat.ethz.ch/pipermail/r-help/2008-September/174475.html
    ind <- upper.tri(sim.mat)
    sim.mat[ind] <- t(sim.mat)[ind]
  }
  
  timer(start.time)
  return(sim.mat)
}
