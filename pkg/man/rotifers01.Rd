\name{rotifers01}
\alias{rotifers01}
\docType{data}
\title{
Rotifer presence-absence on TDWG-level3 regions of the world
}
\description{
These data were extracted from a database of monogonont rotifer species presence records on the geographical units used by the Biodiversity Information Standards (formerly Taxonomic Database Working Group, TDWG; base maps available at http://www.kew.org/science-research-data/kew-in-depth/gis/resources-and-publications/data/tdwg/index.htm). The original data were compiled and published by Fontaneto et al. (2012) in long (narrow, stacked) format. Here they are presented in wide or unstacked format (presence-absence table, obtained with \code{\link{splist2presabs}}), reduced to the species recorded in at least 80 different TDWG - level 3 units, and with abbreviations of the species' names (obtained with \code{\link{spCodes}}). Mind that this is not a complete picture of these species' distributions, due to insufficient sampling in many regions.
}
\usage{data(rotifers01)}
\format{
  Data frame with 369 rows (TDWG - level 3 regions of the world) and 40
  columns:\code{LEVEL3_COD}, \code{ID} and \code{LEVEL_NAME} (from the TDWG 3 map
  attribute table); \code{Lati} and \code{Long} (mean latitude and mean longitude
  of each region); and 35 rotifer species (from \code{Abrigh} on), with names
  abbreviated to first letter of genus + first five letters of specific name.
  \describe{
    \item{}{}
  }
}
\details{
}
\source{
Fontaneto D., Barbosa A.M., Segers H. & Pautasso M. (2012) The 'rotiferologist' effect and other global correlates of species richness in monogonont rotifers. Ecography, 35: 174-182.
}
\references{
}
\examples{
data(rotifers01)

head(rotifers01)
}
\keyword{datasets}
