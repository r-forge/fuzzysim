\name{distPres}
\alias{distPres}
\title{
(Inverse) distance to the nearest presence
}
\description{
This function takes a matrix or data frame containing species presence (1) and absence (0) records and their spatial coordinates (optionally also a pre-computed pairwise distance matrix for all localities), and computes the (inverse) distance from each locality to the nearest presence locality for each species. It can be used to compute a simple spatial interpolation model of a species' distribution (e.g. Barbosa 2015, Areias-Guerreiro et al. 2016).
}
\usage{
distPres(data, sp.cols, coord.cols = NULL, id.col = NULL,
dist.mat = NULL, CRS = NULL, method = "auto", suffix = "_D",
p = 1, inv = TRUE, verbosity = 2)
}
\arguments{
  \item{data}{
a matrix or data frame containing, at least, two columns with spatial coordinates, and one column per species containing their presence (1) and absence (0) data, with localities in rows.
}
  \item{sp.cols}{
names or index numbers of the columns containing the species presences and absences in 'data'. It must contain only zeros (0) for absences and ones (1) for presences.
}
  \item{coord.cols}{
names or index numbers of the columns containing the spatial coordinates in 'data' (x and y, or longitude and latitude, in this order!).
}
  \item{id.col}{
optionally, the name or index number of a column (to be included in the output) containing locality identifiers in 'data'.
}
  \item{dist.mat}{
optional pre-computed pairwise distance matrix for the localities in 'data'. If provided, arguments 'CRS' and 'method' are not used.
}
\item{CRS}{
(if 'dist.mat' is NULL) argument to pass to \code{\link{distMat}} specifying coordinate reference system of the 'coord.cols' in 'data'.
}
  \item{method}{
(if 'dist.mat' is NULL) argument to pass to \code{\link{distMat}}. The default is "auto".
}
  \item{suffix}{
character indicating the suffix to add to the distance columns in the resulting data frame. The default is \code{"_D"}.
}
  \item{p}{
the power to which distance should be raised. The default is 1; use 2 or higher if you want more conservative distances.
}
  \item{inv}{
logical value indicating whether distance should be inverted (i.e., converted to proximity), in which case they will be standardized to vary between 0 and 1, and then subtracted from 1. The output will also vary between 0 and 1, with higher values meaning closer to a presence, and 1 meaning presence. The default is \code{TRUE}, which can be used as a fuzzy version of presence-absence (for using e.g. with \code{\link{fuzSim}} and \code{\link{simMat}}).
}
  \item{verbosity}{
integer specifying the amount of messages to display along the process. The default is 2, for the maximum amount of messages available.
}
}
\value{
This function returns a matrix or data frame containing the identifier column (if provided in 'id.col') and one column per species containing the distance (inverse by default, i.e. proximity) from each locality to the nearest presence of that species.
}
\references{
Areias-Guerreiro J., Mira A. & Barbosa A.M. (2016) How well can models predict changes in species distributions? A 13-year-old otter model revisited. Hystrix - Italian Journal of Mammalogy, in press. DOI: http://dx.doi.org/10.4404/hystrix-27.1-11867

Barbosa A.M. (2015) fuzzySim: applying fuzzy logic to binary similarity indices in ecology. Methods in Ecology and Evolution, 6: 853-858
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{dist}}
}
\examples{
data(rotif.env)

head(rotif.env)

names(rotif.env)


\dontrun{
# distance to presence (in meters):

rotifers.dist <- distPres(rotif.env, sp.cols = 18:47,
coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 1,
inv = FALSE, suffix = "_D", CRS = "EPSG:4326")

head(rotifers.dist)


# inverse squared distance to presence (proximity):

rotifers.invd2 <- distPres(rotif.env, sp.cols = 18:47,
coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 2,
inv = TRUE, suffix = "_iDsq", CRS = "EPSG:4326")

head(rotifers.invd2)
}
}
\keyword{ spatial }
\keyword{ models }
