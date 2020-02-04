\name{rotifers}
\alias{rotifers}
\docType{data}
\title{
Rotifer species on TDWG level 4 regions of the world
}
\description{
These data were extracted from a database of monogonont rotifer species records on the geographical units used by the Biodiversity Information Standards (formerly Taxonomic Database Working Group, TDWG: \url{https://www.tdwg.org}). The original data were compiled and published by Fontaneto et al. (2012) for all TDWG levels. Here they are reduced to the TDWG - level 4 units and to the species recorded in at least 100 (roughly one third) of these units. Mind that this is not a complete picture of these species' distributions, due to insufficient sampling in many regions.
}
\usage{data("rotifers")}
\format{
  A data frame with 3865 observations on the following 2 variables.
  \describe{
    \item{\code{TDWG4}}{a factor with 274 levels corresponding to the code
    names of the TDWG level 4 regions in which the records were taken}
    \item{\code{species}}{a factor with 30 levels corresponding to the names of
    the (sub)species recorded in at least 100 different TDWG level 4 regions}
  }
}
\source{
Fontaneto D., Barbosa A.M., Segers H. & Pautasso M. (2012) The 'rotiferologist' effect and other global correlates of species richness in monogonont rotifers. Ecography, 35: 174-182.
}
\examples{
data(rotifers)

head(rotifers, 10)
}
\keyword{datasets}
