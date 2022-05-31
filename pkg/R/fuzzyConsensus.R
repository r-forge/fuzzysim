fuzzyConsensus <- function(data, fav.cols = 1:ncol(data), weights = "PCA1", simplif = TRUE, plot = TRUE, biplot = TRUE, verbosity = 2, do.par = TRUE) {
  # version 1.0 (25 May 2022)
  
  if (do.par) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mar = c(4.1, 4.1, 4.1, 2.1))
    if (sum(plot, biplot) == 2) par(mfrow = c(1, 2))
  }
  
  dat <- data[ , fav.cols, drop = FALSE]
  if (ncol(dat) == 1) return(dat)
  
  na_rows <- unique(which(is.na(dat), arr.ind = TRUE)[ , "row"])
  
  if (length(na_rows) > 0) dat_finite <- dat[-na_rows, ]
  else dat_finite <- dat
  if (any(dat_finite < 0) || any(dat_finite > 1)) stop ("Input contains values that are not between 0 and 1, as fuzzy membership must be.")
  
  if (weights == "PCA1") {
    pca <- prcomp(dat_finite, center = FALSE, scale. = FALSE)
    rot <- pca$rotation[ , "PC1"]
    wmn <- apply(dat, 1, weighted.mean, w = rot)
    if (verbosity > 0) {
      prop <- summary(pca)$importance["Proportion of Variance", "PC1"]
      prop <- 100 * round(prop, 3)
      cat("PCA axis 1 accounts for ", prop, "% of the variance in the input columns.\n", sep = "")
      #cor(abs(pca$x[ , "PC1"]), wmn)
    }
    if (plot) plot(pca, main = "PCA")
    if (biplot) biplot(pca)
  } else stop ("Invalid 'weights' argument.")
  
  if (simplif) return(wmn)
  else return(list(pca = pca, consensus = wmn))
}
