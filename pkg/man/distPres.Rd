\name{distPres}
\alias{distPres}
\title{
(Inverse) distance to the nearest presence
}
\description{
This function takes a matrix or data frame containing species presence (1) and absence (0) data and their spatial coordinates (optionally also a pre-calculated distance matrix between all localities), and calculates the (inverse) distance from each locality to the nearest presence locality for each species.
}
\usage{
distPres(data, sp.cols, coord.cols = NULL, id.col = NULL, dist.mat = NULL, method = "euclidian", suffix = "_D", p = 1, inv = TRUE)
}
\arguments{
  \item{data}{
a matrix or data frame containing, at least, two columns with spatial coordinates, and one column per species containing their presence (1) and absence (0) data, with localities in rows.
}
  \item{sp.cols}{
names or index numbers of the columns containing the species presences and absences in data. Must contain only zeros (0) for absences and ones (1) for presences.
}
  \item{coord.cols}{
names or index numbers of the columns containing the spatial coordinates in data (in this order, x and y, or longitude and latitude).
}
  \item{id.col}{
optionally, the name or index number of a column (to be included in the output) containing locality identifiers in data.
}
  \item{dist.mat}{
optionally, if you do not want distances calculated with any of the methods available in \code{\link{dist}}, you may provide a distance matrix calculated elsewhere for the localities in data.
}
  \item{method}{
the method with which to calculate distances between localities. Available options are those of \code{\link{dist}}. The default is "euclidian".
}
  \item{suffix}{
character indicating the suffix to add to the distance columns in the resulting data frame. The default is "_D".
}
  \item{p}{
the power to which distance should be raised. The default is 1; use 2 or higher if you want to be more conservative.
}
  \item{inv}{
logical value indicating whether distance should be inverted, so that it varies between 0 and 1 and higher values mean closer to presence. The default is \code{TRUE}, which is adequate as a fuzzy version of presence-absence (for using e.g. with \code{\link{fuzSim}} and \code{\link{simMat}}).
}
}
\details{
}
\value{
\code{distPres} returns a matrix or data frame containing the identifier column (if provided in \code{id.col}) and one column per species containing the distance (inverse squared by default) from each locality to the nearest presence of that species.
}
\references{
}
\author{
A. Marcia Barbosa
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist}}
}
\examples{
data(rotifers01)

head(rotifers01)

names(rotifers01)


# calculate plain distance to presence:

rotifers.dist <- distPres(rotifers01, sp.cols = 6:40, coord.cols = c("Long", "Lati"), id.col = 1, p = 1, inv = FALSE, suffix = "_D")

head(rotifers.dist)


# calculate inverse squared distance to presence:

rotifers.invd2 <- distPres(rotifers01, sp.cols = 6:40, coord.cols = c("Long", "Lati"), id.col = 1, p = 2, inv = TRUE, suffix = "_iDsq")

head(rotifers.invd2)
}
\keyword{ distance }
\keyword{ interpolation }
\keyword{ IDW }