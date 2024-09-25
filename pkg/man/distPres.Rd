\name{distPres}
\alias{distPres}
\title{
(Inverse) distance to the nearest presence
}
\description{
This function takes a matrix or data frame containing species presence (1) and absence (0) data and their spatial coordinates (optionally also a pre-calculated distance matrix between all localities), and calculates the (inverse) distance from each locality to the nearest presence locality for each species.
}
\usage{
distPres(data, sp.cols, coord.cols = NULL, id.col = NULL,
dist.mat = NULL, CRS = NULL, method = "euclidean", suffix = "_D",
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
names or index numbers of the columns containing the spatial coordinates in 'data' (in this order, x and y, or longitude and latitude).
}
  \item{id.col}{
optionally, the name or index number of a column (to be included in the output) containing locality identifiers in 'data'.
}
  \item{dist.mat}{
optionally, if you do not want distances calculated with any of the methods available in the \code{\link{dist}} function of package \pkg{stats}, you may provide a distance matrix calculated elsewhere for the localities in 'data'. This is especially advisable for large geographic extents, where you will get much more accurate distances with a function that considers the curvature of the Earth -- e.g., function \code{distance} in package \pkg{terra}, or function \code{st_distance} in package \pkg{sf}.
}
\item{CRS}{
coordinate reference system of the 'coord.cols' in 'data', in one of the following formats: WKT/WKT2, <authority>:<code>, or PROJ-string notation (see \code{\link[terra:crs]{terra::crs()}}). Ignored if 'dist.mat' is provided. Otherwise, if 'CRS' is provided and the 'terra' package is installed, distances are computed with \code{\link[terra:distance]{terra::distance()}}, thus accounting for the curvature of the Earth.
}
  \item{method}{
(if neither 'dist.mat' nor 'CRS' are provided) the method with which to compute the distances between localities. Available options are those of \code{\link{dist}}. The default is "euclidean".
}
  \item{suffix}{
character indicating the suffix to add to the distance columns in the resulting data frame. The default is \code{"_D"}.
}
  \item{p}{
the power to which distance should be raised. The default is 1; use 2 or higher if you want more conservative distances.
}
  \item{inv}{
logical value indicating whether distance should be inverted, i.e. standardized to vary between 0 and 1 and then subtracted from 1, so that it varies between 0 and 1 and higher values mean closer to presence. The default is \code{TRUE}, which is adequate as a fuzzy version of presence-absence (for using e.g. with \code{\link{fuzSim}} and \code{\link{simMat}}). In this case, presences maintain the value 1, and inverse distance to presence is calculated only for absence localities.
}
  \item{verbosity}{
integer specifying the amount of messages to display along the process. The default is 2, for the maximum amount of messages available.
}
}
\details{
This function can be used to calculate a simple spatial interpolation model of a species' distribution (e.g. Barbosa 2015, Areias-Guerreiro et al. 2016).
}
\value{
This function returns a matrix or data frame containing the identifier column (if provided in 'id.col') and one column per species containing the distance (inverse by default) from each locality to the nearest presence of that species.
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


# calculate plain distance to presence:

rotifers.dist <- distPres(rotif.env, sp.cols = 18:47,
coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 1,
inv = FALSE, suffix = "_D")

head(rotifers.dist)


# calculate inverse squared distance to presence:

rotifers.invd2 <- distPres(rotif.env, sp.cols = 18:47,
coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 2,
inv = TRUE, suffix = "_iDsq")

head(rotifers.invd2)
}
\keyword{ spatial }
\keyword{ models }
