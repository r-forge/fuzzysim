\name{cleanCoords}
\alias{cleanCoords}
\title{
  Clean coordinates
}
\description{
  This function takes a data frame with species occurrences and removes the rows whose coordinates do not pass a set of user-specified filters (see Arguments
). Row names are inheritted from the input data frame, i.e. if row "2" is cleaned out, output rownames will be c("1", "3", ...).
}
\usage{
  cleanCoords(data, coord.cols = NULL, uncert.col = NULL, abs.col = NULL,
  year.col = NULL, rm.dup = !is.null(coord.cols),
  rm.missing.any = !is.null(coord.cols), rm.missing.both = !is.null(coord.cols),
  rm.zero.any = !is.null(coord.cols), rm.zero.both = !is.null(coord.cols),
  rm.equal = !is.null(coord.cols),   rm.imposs = !is.null(coord.cols),
  rm.imprec.any = !is.null(coord.cols), rm.imprec.both = !is.null(coord.cols),
  imprec.digits = 0, rm.uncert = !is.null(uncert.col), uncert.limit = 50000,
  uncert.na.pass = TRUE, rm.abs = !is.null(abs.col), year.min = NULL,
  year.na.pass = TRUE, plot = TRUE, extend = 0.1)
}
\arguments{
  \item{data}{an object inheriting class 'data.frame' with the spatial coordinates to be cleaned, or a 'SpatVector' of points.}
  \item{coord.cols}{character or integer vector of length 2, with either the names or the positions of the columns that contain the spatial coordinates in 'data' - in this order, LONGitude and LATitude, or x and y. Can be left NULL if 'data' is a 'SpatVector', in which case the coordinates will be extracted with terra::crds().}
  \item{uncert.col}{character or integer vector of length 1, with either the name or the position of the column that reports spatial uncertainty in 'data' (e.g., in GBIF this column is usually named "coordinateUncertaintyInMeters").}
  \item{abs.col}{character or integer vector of length 1, with either the name or the position of the column that specifies whether the species is present or absent (e.g., in GBIF this column is usually named "occurrenceStatus"). If povided, rows where the value in this column is not NA and is either (case-insensitive) "absent" or "ausente" will be removed.}
  \item{year.col}{character or integer vector of length 1, with either the name or the position of the column that specifies the year in which the observation was made (e.g., in GBIF this column is usually named "year").}
  \item{rm.dup}{logical, whether to remove rows with exactly the same location, i.e. the same pair of longitude-latitude coordinates. The default is TRUE if 'coord.cols' is not NULL, and FALSE otherwise.}
  \item{rm.missing.any}{logical, whether to remove rows where at least one of the coordinates is NA. The default is TRUE if 'coord.cols' is not NULL, and FALSE otherwise.}
  \item{rm.missing.both}{logical, whether to remove rows where both coordinates are NA. The default is TRUE if 'coord.cols' is not NULL and FALSE otherwise, but it is not used (as it is redundant) if rm.missing.any=TRUE.}
  \item{rm.zero.any}{logical, whether to remove rows where at least one of the coordinates exactly equals zero (which is likely an error). The default is TRUE if 'coord.cols' is not NULL, and FALSE otherwise.}
  \item{rm.zero.both}{logical, whether to remove rows where both coordinates equal zero (which is likely an error). The default is TRUE if 'coord.cols' is not NULL and FALSE otherwise, but it is not used (as it is redundant) if rm.zero.any=TRUE.}
  \item{rm.equal}{logical, whether to remove rows where latitude exactly equals longitude (which is likely an error). The default is TRUE if 'coord.cols' is not NULL, and FALSE otherwise.}
  \item{rm.imposs}{logical, whether to remove rows with impossible coordinates outside planet Earth, i.e. with absolute value >180 for longitude or >90 for latitude. The default is TRUE if 'coord.cols' is not NULL, and FALSE otherwise. Note that this is only valid for unprojected angular coordinates in geographic degrees.}
  \item{rm.imprec.any}{logical, whether to remove rows where at least one of the coordinates is imprecise, i.e. has no more decimal places than 'imprec.digits'. The default is TRUE if 'coord.cols' is not NULL and FALSE otherwise, but note this is normally only relevant for unprojected geographical coordinates in degrees; if your coordinates are in meters, they are usually precise enough without decimal places, so you should probably set this argument and the next to FALSE.}
  \item{rm.imprec.both}{logical, whether to remove rows where both coordinates are imprecise, i.e. have no more decimal places than 'imprec.digits'. The default is TRUE if 'coord.cols' is not NULL and FALSE otherwise, but it is not used (as it is redundant) if rm.imprec.any=TRUE. See 'rm.imprec.any' above for important details.}
  \item{imprec.digits}{integer, maximum number of digits to consider that a coordinate is imprecise. Used only if 'rm.imprec.any' or 'rm.imprec.both' is TRUE. The default is 0, for eliminating coordinates with no more than zero decimal places.}
  \item{rm.uncert}{logical, whether to remove rows where the value in 'uncert.col' is higher than 'uncert.limit'. The default is TRUE if 'uncert.col' is not NULL, and FALSE otherwise.}
  \item{uncert.limit}{lnumeric, threshold value for 'uncert.col'. If rm.uncert=TRUE and 'uncert.col' is provided, rows with values above this will be excluded. The default is 50,000, i.e. 50 km if the values in 'uncert.col' are in meters.}
  \item{uncert.na.pass}{logical, whether rows with NA in 'uncert.col' should be kept as having no uncertainty. The default is TRUE.}
  \item{rm.abs}{logical, whether to remove rows where the value in 'abs.col' is (case-insensitive) 'absent'. The default is TRUE if 'abs.col' is not NULL, and FALSE otherwise.}
  \item{year.min}{positive integer specifying the minimum (earliest) value admitted for the year column. The default is NULL (no limit).}
  \item{year.na.pass}{logical, whether rows with NA in 'year.col' should be kept as if fulfilling the year.min criterion. The default is TRUE.}
  \item{plot}{logical value specifying whether to plot the result. The default is TRUE.}
  \item{extend}{numeric value specifying the proportion of the input coordinates range by which to increase the extent of the output plot (if plot=TRUE). The default is 0.1, i.e. 10\%.}
}
\details{
  This function applies some basic cleaning procedures for species occurrence data, removing some of the most common errors in biodiversity databases. It is inspired by a few functions (namely 'coord_incomplete', 'coord_imprecise', 'coord_impossible', 'coord_unlikely' and 'coord_uncertain') that were present in the 'scrubr' package by Scott Chamberlain, which was archived (https://github.com/ropensci-archive/scrubr). It implements some additional cleaning procedures, such as removal of records of absence and records older than a given year. It also maps the result.
}
\value{
  This function returns a data frame of the input 'data' (or a 'SpatVector' if this matches the input) after excluding the rows that met the specified removal criteria. The row names match the original ones in 'data', at least if 'data' is of class 'data.frame'. Messages are displayed in the console saying how many rows passed each removal filter. If plot=TRUE (the default), a plot is also displayed with the selected points (blue dots) overlaid to the excluded points (red "x").
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link{gridRecords}}
}
\examples{
  \dontrun{
    # you can run these examples if you have the 'geodata' package installed

    # download some species occurrences from GBIF:
    occ <- geodata::sp_occurrence(genus = "Orycteropus", species = "afer",
    fixnames = FALSE)

    # clean occurrences:
    names(occ)
    occ_clean <- cleanCoords(occ,
                      coord.cols = c("decimalLongitude", "decimalLatitude"),
                      abs.col = "occurrenceStatus",
                      uncert.col = "coordinateUncertaintyInMeters",
                      uncert.limit = 10000,  # 10 km tolerance
                      year.col = "year", year.min = 1950)
  }
}
