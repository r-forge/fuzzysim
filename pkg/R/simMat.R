simMat <-
function(data, method) {
  stopifnot(data >= 0, data <= 1, 
            method %in% c('Baroni', 'Jaccard', 'Sorensen'))
  n.subjects <- ncol(data)
  quarter <- round(n.subjects / 4)
  half <- round(n.subjects / 2)
  threequarters <- half + quarter
  sim.mat <- matrix(nrow = n.subjects, ncol = n.subjects, 
                    dimnames = list(colnames(data), colnames(data)))
  message ("Calculating pair-wise similarities, please wait...")
  for (i in 1:n.subjects) for (j in 1:n.subjects) {
    sim.mat[i, j] <- fuzSim(x = data[ , i], y = data[ , j], method = method)
    if(i == quarter & j == quarter) message ("25% done...")
    if(i == half & j == half) message ("50% done...")
    if(i == threequarters & j == threequarters) message ("75% done...")
    if(i == n.subjects & j == n.subjects) message ("Finished!")
  }
  return(sim.mat)
}
