distPres <- function(data, sp.cols, coord.cols = NULL, id.col = NULL, dist.mat = NULL, CRS = NULL, method = "euclidean", suffix = "_D", p = 1, inv = TRUE, verbosity = 2) {
  # version 2.4 (25 Sep 2024)

  data <- as.data.frame(data)
  stopifnot(
    as.matrix(data[ , sp.cols]) %in% c(NA, 0, 1),
    length(sp.cols) > 0,
    length(sp.cols) <= ncol(data) - length(coord.cols) - length(id.col),
    !is.null(coord.cols) | !is.null(dist.mat),
    is.null(coord.cols) | length(coord.cols) == 2,
    is.null(coord.cols) | coord.cols %in% 1:ncol(data) | coord.cols %in% colnames(data),
    is.numeric(as.matrix(data[ , coord.cols])),
    is.null(id.col) | id.col %in% 1:ncol(data) | id.col %in% colnames(data),
    is.null(dist.mat) | nrow(dist.mat) == nrow(data)
  )

  if (is.null(dist.mat)) {

    trr <- "terra" %in% .packages(all.available = TRUE)
    if (verbosity > 0 && isFALSE(trr)) message("NOTE: without the 'terra' package installed,\n distance is both slower and less accurate.")

    if (is.null(CRS)) {
      if (verbosity > 0) warning("Distances may be inaccurate when CRS is not provided.")
      CRS <- "local"  # arbitrary Cartesian space
    }

    if (!is.null(CRS) && isTRUE(trr)) {
      # if (verbosity > 1) message("Computing pairwise distances...")

      # ll <- terra::is.lonlat(v, perhaps = TRUE, warn = TRUE)
      # dist.mat <- terra::distance(data[ , coord.cols], lonlat = ll)

      v <- terra::vect(as.matrix(data[ , coord.cols]), crs = CRS)
      dist.mat <- terra::distance(v)

      # if (verbosity > 1) message("Computing nearest distances...")
      # crd_names <- if (is.character(coord.cols)) coord.cols else colnames(data)[coord.cols]
      # p <- data[ , sp.cols] == 1
      # a <- data[ , sp.cols] == 0
      # pres_pts <- terra::vect(as.matrix(data[p, ]), geom = crd_names, crs = CRS)
      # abs_pts <- terra::vect(as.matrix(data[a, ]), geom = crd_names, crs = CRS)
      # nearest_pres <- terra::nearest(abs_pts, pres_pts)
      # data[p, "distpres"] <- 0
      # data[a, "distpres"] <- nearest_pres$distance

    } else {
      if (verbosity > 0) message("NOTE: with 'CRS' not provided or 'terra' package not installed,\n distances don't consider the curvature of the Earth")
      dist.mat <- stats::dist(data[ , coord.cols], method = method)
    }
  }

  dist.mat <- as.matrix(dist.mat)
  sp.data <- data[ , sp.cols, drop = FALSE]
  n.obs <- nrow(dist.mat)
  n.subjects <- length(sp.cols)

  #if (n.subjects == 1) {
  #  sp.data <- as.matrix(sp.data)
  #  colnames(sp.data) <- colnames(data)[sp.cols]
  #}

  pres.dist.mat <- matrix(nrow = n.obs,
                          ncol = n.subjects,
                          dimnames = list(rownames(dist.mat),
                                          colnames(sp.data)))

  for (o in 1:n.obs) for (s in 1:n.subjects) {
    pres.dist.mat[o, s] <- min(dist.mat[sp.data[ , s] == 1, o])
  }

  #if (inv | p != 1) pres.dist.mat <- pres.dist.mat + 1  # avoids result being smaller than the original when distance < 1 (and larger than the original otherwise)
  pres.dist.mat <- pres.dist.mat ^ p

  if (inv) {
    #pres.dist.mat[!is.finite(pres.dist.mat)] <- max(is.finite(pres.dist.mat))  # gives 1 to the inverse distance to presence at presence sites
    d <- pres.dist.mat  # new (to shorten line below)
    pres.dist.mat <- (d - min(d, na.rm = T)) / (max(d, na.rm = T) - min(d, na.rm = T))  # new: standardize to 01
    pres.dist.mat <- 1 - pres.dist.mat  # new
  }

  colnames(pres.dist.mat) <- paste0(colnames(sp.data), suffix)

  if (!is.null(id.col)) {
    pres.dist.mat <- data.frame(data[ , id.col], pres.dist.mat)
    if (is.character(id.col)) colnames(pres.dist.mat)[1] <- id.col
    else colnames(pres.dist.mat)[1] <- colnames(data)[id.col]
  }

  pres.dist.mat
}
