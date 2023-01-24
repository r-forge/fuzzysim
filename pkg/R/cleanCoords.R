#' Title Clean coordinates
#'
#' @param data an object inheriting class 'data.frame' containing the spatial coordinates to be cleaned.
#' @param uncert.col character or integer vector of length 1, with either the name or the position of the column that reports spatial uncertainty in 'data' (e.g., in GBIF this column is usually named "coordinateUncertaintyInMeters").
#' @param rm.dup logical, whether to remove rows with exactly the same pair of coordinates. The default is TRUE.
#' @param rm.equal logical, whether to remove rows with exactly the same pair of coordinates, i.e. where latitude = longitude. The default is TRUE.
#' @param rm.imposs logical, whether to remove rows with coordinates outside planet Earth, i.e. with absolute value >180 for longitude or >90 for latitude. The default is TRUE.
#' @param rm.missing.any logical, whether to remove rows where at least one of the coordinates is NA. The default is TRUE.
#' @param rm.missing.both logical, whether to remove rows where both coordinates are NA. The default is TRUE, but it is not used if rm.missing.any=TRUE.
#' @param rm.zero.any logical, whether to remove rows where at least one of the coordinates equals zero (which is usually an error). The default is TRUE.
#' @param rm.zero.both logical, whether to remove rows where both coordinates equal zero (which is usually an error). The default is TRUE, but it is not used if rm.zero.any=TRUE.
#' @param rm.imprec.any logical, whether to remove rows where at least one of the coordinates is imprecise, i.e. has at most the number of decimal places specified in 'imprec.digits'. The default is TRUE, but note this normally applies to coordinates in geographical coordinates in sexagesimal degrees; if your coordinates are in meters, they are usually precise enough without decimal places, so you should probably set this argument and the next to FALSE.
#' @param rm.imprec.both logical, whether to remove rows where both coordinates are imprecise. The default is TRUE, but it is not used if rm.imprec.any=TRUE. See 'rm.imprec.any' above for important details.
#' @param imprec.digits integer, maximum number of digits to consider that a coordinate is imprecise. The default is 0, for eliminating coordinates with no more than zero decimal places.
#' @param rm.uncert logical, whether to remove rows where the value in 'uncert.col' is higher than 'uncert.limit'. The default is TRUE if 'uncert.col' is not NULL, and FALSE otherwise.
#' @param uncert.limit numeric, threshold value for 'uncert.col'. If rm.uncert=TRUE and 'uncert.col' is provided, rows with values above this will be excluded. The default is 50,000, i.e. 50 km if the values in 'uncert.col' are in meters.
#' @param uncert.na.pass logical, whether rows with NA in 'uncert.col' should be kept as having no uncertainty. The default is TRUE.
#'
#' @return This function returns a data frame of the input 'data' without the rows that met the specified removal criteria. The row names are the same as the original ones in 'data'. Messages are displayed in the console saying how many rows passed each removal filter.
#' @export
#'
#' @examples

cleanCoords <- function(data, coord.cols, uncert.col = NULL, rm.dup = TRUE, rm.equal = TRUE, rm.imposs = TRUE, rm.missing.any = TRUE, rm.missing.both = TRUE, rm.zero.any = TRUE, rm.zero.both = TRUE, rm.imprec.any = TRUE, rm.imprec.both = TRUE, imprec.digits = 0, rm.uncert = !is.null(uncert.col), uncert.limit = 50000, uncert.na.pass = TRUE, plot = TRUE) {
  # version 1.2 (24 Jan 2023)

  stopifnot(
    inherits(data, "data.frame"),
    length(coord.cols) == 2,
    (is.character(coord.cols) && all(coord.cols %in% names(data))) || (is.integer(coord.cols) && all(coord.cols %in% 1:ncol(data))),
    is.null(uncert.col) || length(uncert.col) == 1,
    # !rm.uncert || !is.null(uncert.col),  # too cryptic, replaced below
    is.null(uncert.col) || (is.character(uncert.col) && uncert.col %in% names(data)) || (is.integer(uncert.col) && uncert.col %in% 1:ncol(data))
    )

  if (rm.uncert && is.null(uncert.col)) stop("'rm.uncert=TRUE' requires specifying 'uncert.col'.")

  data <- as.data.frame(data)
  if (plot) data.in <- data

  coords <- data[ , coord.cols]
  names(coords) <- c("lon", "lat")
  message(nrow(coords), " rows in input data")

  if (rm.dup) {
    coords <- coords[!duplicated(coords), ]
    message(nrow(coords), " rows after 'rm.dup'")
  }

  if (rm.equal) {
    coords <- subset(coords, coords$lon != coords$lat)
    message(nrow(coords), " rows after 'rm.equal'")
  }

  if (rm.imposs) {
    coords <- subset(coords, abs(coords$lon) <= 180 & abs(coords$lat) <= 90)
    message(nrow(coords), " rows after 'rm.imposs'")
  }

  if (rm.missing.any) {
    coords <- na.omit(coords)
    message(nrow(coords), " rows after 'rm.missing.any'")
  } else if (rm.missing.both) {
    coords <- subset(coords, !is.na(coords$lon) & !is.na(coords$lat))
    message(nrow(coords), " rows after 'rm.missing.both'")
  }

  if (rm.zero.any) {
    coords <- subset(coords, coords$lon != 0 | coords$lat != 0)
    message(nrow(coords), " rows after 'rm.zero.any'")
  } else if (rm.zero.both) {
    coords <- subset(coords, coords$lon != 0 & coords$lat != 0)
    message(nrow(coords), " rows after 'rm.zero.both'")
  }

  if (rm.imprec.any) {
    # coords <- coords[grepl("[0-9]+\\.[0-9]+", coords$lon) | grepl("[0-9]+\\.[0-9]+", coords$lat), ]  # adapted from 'scrubr::coord_imprecise', removes only integer coords (no 'digits' option)
    coords <- subset(coords, coords$lon != round(coords$lon, imprec.digits) & coords$lat != round(coords$lat, imprec.digits))
    message(nrow(coords), " rows after 'rm.imprec.any'")
  } else if (rm.imprec.both) {
    # coords <- coords[grepl("[0-9]+\\.[0-9]+", coords$lon) & grepl("[0-9]+\\.[0-9]+", coords$lat), ]  # adapted from 'scrubr::coord_imprecise', removes only integer coords (no 'digits' option)
    coords <- subset(coords, coords$lon != round(coords$lon, imprec.digits) | coords$lat != round(coords$lat, imprec.digits))
    message(nrow(coords), " rows after 'rm.imprec.both'")
  }

  data <- data[rownames(coords), ]

  if (rm.uncert) {
    uncert <- data[ , uncert.col]
    if (uncert.na.pass) uncert[is.na(uncert)] <- 0
    accurate <- uncert <= uncert.limit
    coords <- coords[sapply(accurate, isTRUE), ]
    message(nrow(coords), " rows after 'rm.uncert' (with uncert.limit=", uncert.limit, " and uncert.na.pass=", uncert.na.pass, ")")
  }

  data <- data[rownames(coords), ]

  if (plot) {
    # if ("terra" %in% .packages()) {
    #   data.in <- vect(data.in, geom = coord.cols)
    #   data <- vect(data, geom = coord.cols)
    # }
    xrange <- range(data.in[ , coord.cols[1]], na.rm = TRUE)
    yrange <- range(data.in[ , coord.cols[2]], na.rm = TRUE)
    plot(data.in[ , coord.cols], pch = 4, cex = 0.4, col = "red", xlim = xrange, ylim = yrange)
    points(data[ , coord.cols], pch = 20, cex = 0.8, col = "blue")
  }

  return(data)

}
