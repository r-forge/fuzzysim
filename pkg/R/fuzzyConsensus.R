fuzzyConsensus <- function(data, weights = "PCA1", simplif = TRUE, plot = TRUE, biplot = FALSE, verbosity = 2, do.par = TRUE) {
  # version 1.3 (7 Jul 2022)
  
  data_in <- data
  
  if (do.par) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mar = c(4.1, 4.1, 4.1, 2.1))
    if (sum(plot, biplot) == 2) par(mfrow = c(1, 2))
    else par(mfrow = c(1, 1))
  }
  
  data <- as.data.frame(data)  # converts vector, matrix, tibble, SpatRaster
  
  if (ncol(data) == 1) {
    element <- ifelse(inherits(data_in, "SpatRaster"), " layer", " column")
    warning ("'data' has only one", element, "; returning as is.")
    return(data_in)
  }
  
  data_complete <- data[complete.cases(data), ]  # needed for PCA
  
  if (any(data_complete < 0) || any(data_complete > 1)) stop ("Input contains values that are not between 0 and 1, as favourability or fuzzy membership must be.")
  
  if (weights == "PCA1") {
    pca <- prcomp(data_complete, center = FALSE, scale. = FALSE)
    rot <- pca$rotation[ , "PC1"]
    wmn <- apply(data, 1, weighted.mean, w = rot)
    if (verbosity > 0) {
      prop <- summary(pca)$importance["Proportion of Variance", "PC1"]
      perc <- 100 * round(prop, 3)
      cat("PCA axis 1 accounts for ", perc, "% of the variance in the ", ncol(data), " input vectors.\n\n", sep = "")
      #cor(abs(pca$x[ , "PC1"]), wmn)
    }
    if (plot) plot(pca, main = "PCA")
    if (biplot) biplot(pca)
  } else stop ("Invalid 'weights' argument.")
  
  if (inherits(data_in, "SpatRaster")) {
    data_out <- terra::rast(data_in, nlyrs = 1, names = "consensus")
    complete_cases <- complete.cases(terra::values(data_in))
    data_out[complete_cases] <- na.omit(wmn)
    wmn <- data_out
  }
  
  if (simplif) return(wmn)
  return(list(PCA = pca, consensus = wmn))
}
