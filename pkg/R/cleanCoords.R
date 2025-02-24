cleanCoords <- function(data, coord.cols = NULL, uncert.col = NULL, abs.col = NULL, year.col = NULL, rm.dup = !is.null(coord.cols), rm.equal = !is.null(coord.cols), rm.imposs = !is.null(coord.cols), rm.missing.any = !is.null(coord.cols), rm.missing.both = !is.null(coord.cols), rm.zero.any = !is.null(coord.cols), rm.zero.both = !is.null(coord.cols), rm.imprec.any = !is.null(coord.cols), rm.imprec.both = !is.null(coord.cols), imprec.digits = 0, rm.uncert = !is.null(uncert.col), uncert.limit = 50000, uncert.na.pass = TRUE, rm.abs = !is.null(abs.col), year.min = NULL, year.na.pass = TRUE, plot = TRUE, extend = 0.1) {
  # version 1.7 (21 Jan 2025)

  stopifnot(
    inherits(data, "data.frame") || inherits(data, "SpatVector"),
    inherits(data, "SpatVector") || is.null(coord.cols) || length(coord.cols) == 2,
    inherits(data, "SpatVector") || is.null(coord.cols) || (is.character(coord.cols) && all(coord.cols %in% names(data))) || (is.integer(coord.cols) && all(coord.cols %in% 1:ncol(data))),
    is.null(uncert.col) || length(uncert.col) == 1,
    # !rm.uncert || !is.null(uncert.col),  # too cryptic, replaced below
    is.null(uncert.col) || (is.character(uncert.col) && uncert.col %in% names(data)) || (is.integer(uncert.col) && uncert.col %in% 1:ncol(data)),
    is.null(abs.col) || (is.character(abs.col) && abs.col %in% names(data)) || (is.integer(abs.col) && abs.col %in% 1:ncol(data))
  )

  if (inherits(data, "SpatVector")) {
    if (isFALSE(terra::is.points(data)))
      stop ("If 'data' is of class 'SpatVector', its 'geomtype' must be 'points'.")
    data.sv.in <- data
    if (!is.null(coord.cols)) {
      warning("'coord.cols' argument ignored, as 'data' is a spatial object,\n  so coordinates are taken from that")
    }
      data <- data.frame(data.frame(data), terra::crds(data))
      last.cols <- c(ncol(data) - 1, ncol(data))
      coord.cols <- names(data)[last.cols]
  } else {
    data.sv.in <- NULL
  }

  data <- as.data.frame(data)  # for tibbles etc.

  message(nrow(data), " rows in input data")

  if (plot)
    data.in <- data


  coord.ops <- c("rm.dup", "rm.equal", "rm.imposs", "rm.missing.any", "rm.missing.both", "rm.zero.any", "rm.zero.both", "rm.imprec.any", "rm.imprec.both")
  for (o in coord.ops) {
    if (isTRUE(get(o)) && is.null(coord.cols))  stop(paste0("'", o, "=TRUE' requires specifying 'coord.cols'."))
  }


  if (rm.uncert && is.null(uncert.col)) stop("'rm.uncert=TRUE' requires specifying 'uncert.col'.")

  if (rm.abs && is.null(abs.col)) stop("'rm.abs=TRUE' requires specifying 'abs.col'.")

  if (!is.null(year.min) && is.null(year.col)) stop("applying 'year.min' requires specifying 'year.col'.")


  if (is.null(coord.cols)) {
    coords <- data.frame()
    plot <- FALSE
  } else {
    coords <- data[ , coord.cols]
    names(coords) <- c("lon", "lat")
  }

  if (nrow(coords) > 0 && rm.dup) {
    coords <- coords[!duplicated(coords), ]
    message(nrow(coords), " rows after 'rm.dup'")
  }

  if (nrow(coords) > 0 && rm.equal) {
    coords <- subset(coords, coords$lon != coords$lat)
    message(nrow(coords), " rows after 'rm.equal'")
  }

  if (nrow(coords) > 0 && rm.imposs) {
    coords <- subset(coords, abs(coords$lon) <= 180 & abs(coords$lat) <= 90)
    message(nrow(coords), " rows after 'rm.imposs'")
  }

  if (nrow(coords) > 0 && rm.missing.any) {
    coords <- na.omit(coords)
    message(nrow(coords), " rows after 'rm.missing.any'")
  } else if (nrow(coords) > 0 && rm.missing.both) {
    coords <- subset(coords, !is.na(coords$lon) & !is.na(coords$lat))
    message(nrow(coords), " rows after 'rm.missing.both'")
  }

  if (nrow(coords) > 0 && rm.zero.any) {
    coords <- subset(coords, coords$lon != 0 | coords$lat != 0)
    message(nrow(coords), " rows after 'rm.zero.any'")
  } else if (nrow(coords) > 0 && rm.zero.both) {
    coords <- subset(coords, coords$lon != 0 & coords$lat != 0)
    message(nrow(coords), " rows after 'rm.zero.both'")
  }

  if (nrow(coords) > 0 && rm.imprec.any) {
    # coords <- coords[grepl("[0-9]+\\.[0-9]+", coords$lon) | grepl("[0-9]+\\.[0-9]+", coords$lat), ]  # adapted from 'scrubr::coord_imprecise', removes only integer coords (no 'digits' option)
    coords <- subset(coords, coords$lon != round(coords$lon, imprec.digits) & coords$lat != round(coords$lat, imprec.digits))
    message(nrow(coords), " rows after 'rm.imprec.any'")
  } else if (nrow(coords) > 0 && rm.imprec.both) {
    # coords <- coords[grepl("[0-9]+\\.[0-9]+", coords$lon) & grepl("[0-9]+\\.[0-9]+", coords$lat), ]  # adapted from 'scrubr::coord_imprecise', removes only integer coords (no 'digits' option)
    coords <- subset(coords, coords$lon != round(coords$lon, imprec.digits) | coords$lat != round(coords$lat, imprec.digits))
    message(nrow(coords), " rows after 'rm.imprec.both'")
  }

  if (!is.null(coord.cols))  data <- data[rownames(coords), ]

  if (nrow(data) > 0 && rm.uncert) {
    uncert <- data[ , uncert.col]
    if (uncert.na.pass) uncert[is.na(uncert)] <- 0
    accurate <- uncert <= uncert.limit
    data <- data[sapply(accurate, isTRUE), ]
    message(nrow(data), " rows after 'rm.uncert' (with uncert.limit=", uncert.limit, " and uncert.na.pass=", uncert.na.pass, ")")
  }

  if (rm.abs && !is.null(abs.col) && nrow(data) > 0) {
    value <- tolower(as.character(data[ , abs.col]))
    pres <- value != "absent" & value != "ausente"
    data <- data[sapply(pres, isTRUE), ]
    message(nrow(data), " rows after 'rm.abs'")
  }

  if (is.finite(year.min) && !is.null(year.col) && nrow(data) > 0) {
    year_ok <- data[ , year.col] >= year.min
    # if (year.na.pass) year_ok[is.na(data[ , year.col])] <- TRUE
    # else year_ok[is.na(data[ , year.col])] <- FALSE
    year_ok[is.na(data[ , year.col])] <- ifelse(year.na.pass, TRUE, FALSE)
    data <- data[year_ok, ]
    message(nrow(data), " rows after 'year.min' (with year.min=", year.min, " and year.na.pass=", year.na.pass, ")")
  }

  if (is.null(data.sv.in)) {
    if (plot) {
      x_range <- range(data.in[ , coord.cols[1]], na.rm = TRUE)
      y_range <- range(data.in[ , coord.cols[2]], na.rm = TRUE)
      # expand the range by 10% on either side:
      x_range <- x_range + c(-1, 1) * diff(x_range) * extend
      y_range <- y_range + c(-1, 1) * diff(y_range) * extend
      plot(data.in[ , coord.cols], pch = 4, cex = 0.4, col = "red", xlim = x_range, ylim = y_range)
      points(data[ , coord.cols], pch = 20, cex = 0.8, col = "blue")
    }

    return(data)
  }  # end if null data.sv.in


  data.sv.out <- terra::vect(data, geom = coord.cols, crs = terra::crs(data.sv.in), keepgeom = TRUE)

  # if (null.coord.cols) {
    data.sv.out <- data.sv.out[ , -last.cols]
  # }

  if (plot) {
    x_range <- range(terra::crds(data.sv.in)[ , 1], na.rm = TRUE)
    y_range <- range(terra::crds(data.sv.in)[ , 2], na.rm = TRUE)
    # expand the range by 10% on either side:
    x_range <- x_range + c(-1, 1) * diff(x_range) * extend
    y_range <- y_range + c(-1, 1) * diff(y_range) * extend
    terra::plot(data.sv.in, pch = 4, cex = 0.4, col = "red", ext = c(x_range, y_range))
    terra::plot(data.sv.out, pch = 20, cex = 0.8, col = "blue", add = TRUE)
  }

  return(data.sv.out)
}
