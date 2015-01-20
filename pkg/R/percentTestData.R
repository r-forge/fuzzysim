percentTestData <-
function(nvar) {
  # version 1.1 (28 Mar 2013)
  # heuristic ('rule of thumb') of Huberty (1994; in Fielding & Bell 1997) to determine the test/training data ratio for presence-absence models
  # nvar: number of predictor variables
  huberty <- 1 / (1 + sqrt(nvar - 1))
  return(round(100 * huberty, 1))
}
