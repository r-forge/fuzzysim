\name{rotif.env}
\alias{rotif.env}
\docType{data}
\title{
Rotifers and environmental variables on TDWG level 4 regions of the world}
\description{
These data were extracted from a database of monogonont rotifer species presence records on the geographical units used by the Biodiversity Information Standards (formerly Taxonomic Database Working Group, TDWG; base maps available from \url{www.kew.org/science-conservation/research-data/resources/gis-unit/tdwg-world}) and a few environmental (including human and spatial) variables on the same spatial units. The original data were compiled and published by Fontaneto et al. (2012) in long (narrow, stacked) format. Here they are presented in wide or unstacked format (presence-absence table, obtained with the \code{\link{splist2presabs}} function), reduced to the species recorded in at least 100 (roughly one third) different TDWG level 4 units, and with abbreviations of the species' names (obtained with the \code{\link{spCodes}} function). Mind that this is not a complete picture of these species' distributions, due to insufficient sampling in many regions.
}
\usage{data(rotif.env)}
\format{
  A data frame with 291 observations on the following 47 variables.
  \describe{
    \item{\code{TDWG4}}{a factor with 291 levels indicating the abbreviation code of each TDWG4 region}
    \item{\code{LEVEL_NAME}}{a factor with 291 levels indicating the name of each TDWG4 region}
    \item{\code{REGION_NAME}}{a factor with 47 levels indicating the name of the main geographical region to which each TDWG4 level belongs}
    \item{\code{CONTINENT}}{a factor with 9 levels indicating the continent to which each TDWG4 level belongs}
    \item{\code{Area}}{a numeric vector}
    \item{\code{Altitude}}{a numeric vector}
    \item{\code{AltitudeRange}}{a numeric vector}
    \item{\code{HabitatDiversity}}{a numeric vector}
    \item{\code{HumanPopulation}}{a numeric vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{Precipitation}}{a numeric vector}
    \item{\code{PrecipitationSeasonality}}{a numeric vector}
    \item{\code{TemperatureAnnualRange}}{a numeric vector}
    \item{\code{Temperature}}{a numeric vector}
    \item{\code{TemperatureSeasonality}}{a numeric vector}
    \item{\code{UrbanArea}}{a numeric vector}
    \item{\code{Abrigh}}{a numeric vector}
    \item{\code{Afissa}}{a numeric vector}
    \item{\code{Apriod}}{a numeric vector}
    \item{\code{Bangul}}{a numeric vector}
    \item{\code{Bcalyc}}{a numeric vector}
    \item{\code{Bplica}}{a numeric vector}
    \item{\code{Bquadr}}{a numeric vector}
    \item{\code{Burceo}}{a numeric vector}
    \item{\code{Cgibba}}{a numeric vector}
    \item{\code{Edilat}}{a numeric vector}
    \item{\code{Flongi}}{a numeric vector}
    \item{\code{Kcochl}}{a numeric vector}
    \item{\code{Kquadr}}{a numeric vector}
    \item{\code{Ktropi}}{a numeric vector}
    \item{\code{Lbulla}}{a numeric vector}
    \item{\code{Lclost}}{a numeric vector}
    \item{\code{Lhamat}}{a numeric vector}
    \item{\code{Lluna}}{a numeric vector}
    \item{\code{Llunar}}{a numeric vector}
    \item{\code{Lovali}}{a numeric vector}
    \item{\code{Lpatel}}{a numeric vector}
    \item{\code{Lquadr}}{a numeric vector}
    \item{\code{Mventr}}{a numeric vector}
    \item{\code{Ppatul}}{a numeric vector}
    \item{\code{Pquadr}}{a numeric vector}
    \item{\code{Pvulga}}{a numeric vector}
    \item{\code{Specti}}{a numeric vector}
    \item{\code{Tpatin}}{a numeric vector}
    \item{\code{Tsimil}}{a numeric vector}
    \item{\code{Ttetra}}{a numeric vector}
  }
}
\source{
Fontaneto D., Barbosa A.M., Segers H. & Pautasso M. (2012) The 'rotiferologist' effect and other global correlates of species richness in monogonont rotifers. Ecography, 35: 174-182.
}
\examples{
data(rotif.env)

head(rotif.env)
}
\keyword{datasets}
