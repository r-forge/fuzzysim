selectAbsences <- function(data, sp.cols, coord.cols = NULL, CRS = NULL, min.dist = NULL, max.dist = NULL, n = NULL, mult.p = NULL, bias = FALSE, bunch = FALSE, dist.mat = NULL, seed = NULL, plot = !is.null(coord.cols), df = TRUE, verbosity = 2) {
  # version 2.1 (21 Dec 2024)

  if (length(sp.cols) > 1) stop("Sorry, this function is currently implemented for only one 'sp.col' at a time.")
  if (bunch == TRUE) stop("Sorry, 'bunch=TRUE' is still pending implementation.")

  stopifnot(
    inherits(data, "data.frame") || inherits(data, "SpatVector"),
    is.null(coord.cols) || length(coord.cols) == 2,
    is.logical(bias) || inherits(bias, "SpatRaster")
    # is.null(coord.cols) || ((is.character(coord.cols) && all(coord.cols %in% names(data))) || (is.integer(coord.cols) && all(coord.cols %in% 1:ncol(data)))),
    # (is.character(sp.cols) && all(sp.cols %in% names(data))) || (is.integer(sp.cols) && all(sp.cols %in% 1:ncol(data)))
  )

  data_in <- data  # needed e.g. if plot or !df
  data <- as.data.frame(data)  # coerces matrices, tibbles, SpatVectors

  abs.rows <- which(data[ , sp.cols] == 0)
  pres.rows <- which(data[ , sp.cols] == 1)
  n.abs <- length(abs.rows)
  n.pres <- length(pres.rows)
  if (verbosity > 0) cat("\n", n.abs, " absences (and ", n.pres, " presences) in input 'data'.\n", sep = "")

  select <- TRUE
  if (is.null(n) && !is.null(mult.p)) {
    n <- n.pres * mult.p
    if (verbosity > 0) cat("\n", n.pres, " presences x 'mult.p' = ", n, " absences.\n", sep = "")
    n <- round(n, digits = 0)
    if (verbosity > 0 && n > n.abs) {
      cat("\nAbsences not enough to make", mult.p, "times the number of presences; all absences included in output.\n")
      select <- FALSE
    }
  }

  if (select) {
    if (!is.null(min.dist) || !is.null(max.dist) || (is.logical(bias) && isTRUE(bias))) {  #  || isTRUE(bunch)

      if (is.null(coord.cols)) stop("arguments 'min.dist', 'max.dist', 'bias' and 'bunch' require specifying 'coord.cols'.")

      if (verbosity > 0) cat("\nComputing distance to presences (may take long for large datasets)...\n")
      dist.pres <- distPres(data, sp.cols = sp.cols, coord.cols = coord.cols, inv = FALSE, dist.mat = dist.mat, CRS = CRS, verbosity = 1)[, 1]
      if (verbosity > 1) cat("- Distance from input absences to presences ranges between", round(min(dist.pres[abs.rows]), 3), "and", round(max(dist.pres[abs.rows]), 3), "\n")

      if (!is.null(min.dist)) {
        pres.rows <- which(data[ , sp.cols] == 1)
        abs.samp <- which(data[ , sp.cols] == 0 & dist.pres >= min.dist)
        data <- data[c(pres.rows, abs.samp), ]
        dist.pres <- dist.pres[c(pres.rows, abs.samp)]
        pres.rows <- which(data[ , sp.cols] == 1)
        abs.rows <- which(data[ , sp.cols] == 0)
        n.abs <- length(abs.rows)
        if (verbosity > 0) cat("\n", n.abs, " absences selected by 'min.dist'.\n", sep = "")
      }

      if (!is.null(max.dist)) {
        abs.samp <- which(data[ , sp.cols] == 0 & dist.pres <= max.dist)
        data <- data[c(pres.rows, abs.samp), ]
        dist.pres <- dist.pres[c(pres.rows, abs.samp)]
        pres.rows <- which(data[ , sp.cols] == 1)
        abs.rows <- which(data[ , sp.cols] == 0)
        n.abs <- length(abs.rows)
        if (verbosity > 0) cat("\n", n.abs, " absences selected by 'max.dist'.\n", sep = "")
      }
    }  # end if dist.pres needed

    if (!is.null(n) && n < n.abs) {
      if (verbosity > 0 && is.null(mult.p)) cat("\nSelecting n =", n, "absences...\n")

      if (!is.null(seed)) set.seed(seed)

      if (is.logical(bias) && isTRUE(bias)) {
        if (verbosity > 0) cat("\nBiasing the selection of absences towards de vicitiny of presences...\n")
        # inv.dist.pres <- distPres(data, sp.cols = sp.cols, coord.cols = coord.cols, inv = TRUE)[, 1][abs.rows]
        inv <- function(x) 1 - ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
        inv.dist.abs <- inv(dist.pres)[abs.rows]
        abs.samp <- sample(abs.rows, n, replace = FALSE, prob = inv.dist.abs)

      } else if (inherits(bias, "SpatRaster")) {
        if (verbosity > 0) cat("\nBiasing the selection of absences according to the input 'bias' raster...\n")
        if (terra::global(bias, "min", na.rm = TRUE) < 0 || terra::global(bias, "max", na.rm = TRUE) == 0) stop ("'bias' values must be non-negative and not all zeros.")

        bias.abs <- terra::extract(bias, data[abs.rows, coord.cols], cells = FALSE, xy = FALSE, ID = FALSE)[ , 1]
        bias.abs[!is.finite(bias.abs)] <- 0  # zero probability where NA
        abs.samp <- sample(abs.rows, n, replace = FALSE, prob = bias.abs)

      } else {
        abs.samp <- sample(abs.rows, n, replace = FALSE, prob = NULL)
      }

      data <- data[c(pres.rows, abs.samp), ]
      n.abs <- length(abs.samp)

    } else {
      if (verbosity > 0 && (!is.null(n) && is.null(mult.p))) cat("\n'n' is not smaller than the number of absences available, so all absence rows selected.\n")
    }

    if (verbosity > 0) cat("\n", sum(data[ , sp.cols] == 0, na.rm = TRUE), " absences (and ", sum(data[ , sp.cols] == 1, na.rm = TRUE), " presences) in output.\n", sep = "")
  }

  if (plot) {
    if (is.null(coord.cols)) {
      message("'plot=TRUE' requires specifying 'coord.cols'; plot not produced.")

    } else {

      if ("terra" %in% .packages(all.available = TRUE) && !inherits(data_in, "SpatVector")) {  # for better-shaped plot
        data_in_sv <- terra::vect(data_in, geom = coord.cols, keepgeom = TRUE)
        # data_sv <- terra::vect(data, geom = coord.cols, keepgeom = TRUE)
        terra::plot(data_in_sv[data.frame(data_in_sv[ , sp.cols]) == 0, ],
                    ext = terra::ext(data_in_sv),
                    pch = 20, cex = 0.1, col = "orange")
        terra::points(data[data.frame(data[ , sp.cols]) == 0, coord.cols],
                      pch = "-", col = "red")
        terra::points(data[data.frame(data[ , sp.cols]) == 1, coord.cols],
                      pch = "+", col = "blue")
      } else {
        xrange <- range(data_in[ , coord.cols[1]], na.rm = TRUE)
        yrange <- range(data_in[ , coord.cols[2]], na.rm = TRUE)
        plot(data.frame(data_in)[data.frame(data_in)[ , sp.cols] == 0, coord.cols],
             xlim = xrange, ylim = yrange,
             pch = 20, cex = 0.1, col = "orange")
        points(data[data[ , sp.cols] == 0, coord.cols],
               pch = "-", col = "red")
        points(data[data[ , sp.cols] == 1, coord.cols],
               pch = "+", col = "blue")
      }
    }
  }

  if (!df) return(rownames(data.frame(data_in)) %in% rownames(data))
  return(data[order(as.integer(rownames(data))), ])
}
