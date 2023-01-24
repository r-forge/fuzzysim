\name{cleanCoords}
\alias{cleanCoords}
\title{
  Clean coordinates
}
\description{
  This function takes a data frame with species occurrences and removes the rows whose coordinates do not pass a set of user-specified filters.
}
\usage{
  cleanCoords(data, coord.cols, uncert.col = NULL, rm.dup = TRUE,
              rm.equal = TRUE, rm.imposs = TRUE, rm.missing.any = TRUE,
              rm.missing.both = TRUE, rm.zero.any = TRUE, rm.zero.both = TRUE,
              rm.imprec.any = TRUE, rm.imprec.both = TRUE, imprec.digits = 0,
              rm.uncert = !is.null(uncert.col), uncert.limit = 50000,
              uncert.na.pass = TRUE, plot = TRUE)
}
\arguments{
  \item{data}{a matrix or data frame containing, at least, two columns with spatial coordinates, and one column per species containing their presence (1) and absence (0) data, with localities in rows.}
  \item{coord.cols}{character or integer vector of length 2, with either the names or the positions of the columns that contain the spatial coordinates in 'data' - in this order, LONGitude and LATitude, or x and y.}
  \item{uncert.col}{character or integer vector of length 1, with either the name or the position of the column that reports spatial uncertainty in 'data' (e.g., in GBIF this column is usually named "coordinateUncertaintyInMeters").}
  \item{rm.dup}{logical, whether to remove rows with exactly the same pair of coordinates. The default is TRUE.}
  \item{rm.equal}{logical, whether to remove rows with exactly the same pair of coordinates, i.e. where latitude = longitude. The default is TRUE.}
  \item{rm.imposs}{logical, whether to remove rows with coordinates outside planet Earth, i.e. with absolute value >180 for longitude or >90 for latitude. The default is TRUE.}
  \item{rm.missing.any}{logical, whether to remove rows where at least one of the coordinates is NA. The default is TRUE.}
  \item{rm.missing.both}{logical, whether to remove rows where both coordinates are NA. The default is TRUE, but it is not used if rm.missing.any=TRUE.}
  \item{rm.zero.any}{logical, whether to remove rows where at least one of the coordinates equals zero (which is usually an error). The default is TRUE.}
  \item{rm.zero.both}{logical, whether to remove rows where both coordinates equal zero (which is usually an error). The default is TRUE, but it is not used if rm.zero.any=TRUE.}
  \item{rm.imprec.any}{logical, whether to remove rows where at least one of the coordinates is imprecise, i.e. has at most the number of decimal places specified in 'imprec.digits'. The default is TRUE, but note this normally applies to coordinates in geographical coordinates in sexagesimal degrees; if your coordinates are in meters, they are usually precise enough without decimal places, so you should probably set this argument and the next to FALSE.}
  \item{rm.imprec.both}{logical, whether to remove rows where both coordinates are imprecise. The default is TRUE, but it is not used if rm.imprec.any=TRUE. See 'rm.imprec.any' above for important details.}
  \item{imprec.digits}{integer, maximum number of digits to consider that a coordinate is imprecise. The default is 0, for eliminating coordinates with no more than zero decimal places.}
  \item{rm.uncert}{logical, whether to remove rows where the value in 'uncert.col' is higher than 'uncert.limit'. The default is TRUE if 'uncert.col' is not NULL, and FALSE otherwise.}
  \item{uncert.limit}{lnumeric, threshold value for 'uncert.col'. If rm.uncert=TRUE and 'uncert.col' is provided, rows with values above this will be excluded. The default is 50,000, i.e. 50 km if the values in 'uncert.col' are in meters.}
  \item{uncert.na.pass}{logical, whether rows with NA in 'uncert.col' should be kept as having no uncertainty. The default is TRUE.}
  \item{plot}{logical value specifying whether to plot the result. The default is TRUE.}
}
\details{
  This function applies some basic cleaning procedures for species occurrence data, removing some of the most common mistakes in biodiversity data bases. It is inspired by a few functions (namely 'coord_incomplete', 'coord_imprecise', 'coord_impossible', 'coord_unlikely' and 'coord_uncertain') that were present in the 'scrubr' package, which was archived.
}
\value{
  This function returns a data frame of the input 'data' without the rows that met the specified removal criteria. The row names are the same as the original ones in 'data'. Messages are displayed in the console saying how many rows passed each removal filter.
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
    occ <- geodata::sp_occurrence(genus = "Canis", species = "simensis",
    fixnames = FALSE)

    # clean occurrences:
    names(occ)
    occ_clean <- cleanCoords(occ,
                   coord.cols = c("decimalLongitude", "decimalLatitude"),
                   uncert.col = "coordinateUncertaintyInMeters",
                   uncert.limit = 1000)  # 1 km tolerance
  }
}
