simMat <- function(data, method, diag = TRUE, upper = TRUE, verbosity = 2, plot = FALSE, ...) {

  # version 2.4 (3 Oct 2022)

  if (verbosity > 1)  start.time <- Sys.time()

  data <- as.data.frame(data)

  stopifnot(na.omit(data) >= 0,
            na.omit(data) <= 1,
            method %in% c("Jaccard", "Sorensen", "Simpson", "Baroni"))

  n.subjects <- ncol(data)

  sim.mat <- matrix(nrow = n.subjects, ncol = n.subjects,
                    dimnames = list(colnames(data), colnames(data)))

  inds <- triMatInd(sim.mat, lower = TRUE, list = TRUE)

  n.pairs <- length(combn(n.subjects, m = 2)) / 2

  if (verbosity > 1)  message("Computing ", n.pairs, " pair-wise similarities...")

  if (verbosity > 0) {
    progbar <- txtProgressBar(min = 0, max = n.pairs, style = 3, char = "-")
  }

  # quarter <- round(n.pairs / 4)
  # half <- round(n.pairs / 2)
  # threequarters <- half + quarter

  pair <- 0
  for (ind in inds) {
    pair <- pair + 1
    if (verbosity > 0) {
      setTxtProgressBar(progbar, pair)
    }
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

  if (isTRUE(plot)) {
    graphics::image(x = 1:ncol(sim.mat), y = 1:nrow(sim.mat), z = sim.mat,
                    axes = FALSE, xlab = "", ylab = "", ...)
    axis(side = 1, at = 1:ncol(sim.mat), tick = FALSE, labels = colnames(sim.mat), las = 2, cex.axis = 0.6)
    axis(side = 2, at = 1:nrow(sim.mat), tick = FALSE, labels = rownames(sim.mat), las = 2, cex.axis = 0.6)
  }

  if (verbosity > 1) {
    message ("\nFinished!")
    timer(start.time)
  }
  return(sim.mat)
}
