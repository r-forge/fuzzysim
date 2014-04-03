\name{fuzSim}
\alias{fuzSim}
\title{
Fuzzy similarity
}
\description{
Fuzzy similarity, based on a fuzzy version of either Jaccard's (1901) or Baroni-Urbani & Buser's (1976) binary similarity index, between the presence-absence data of two species (or two other binary variables).
}
\usage{
fuzSim(x, y, method)
}
\arguments{
  \item{x}{
a vector of (optionally fuzzy) species presence-absence data, with 1 meaning presence, 0 meaning absence, and values in between meaning fuzzy presence (or the degree to which the locality belongs to the set of species presences; see Zadeh, 1965). Fuzzy presence-absence can be obtained, for example, with functions \code{\link{distPres}} or \code{\link{multTSA}} in this package.
}
  \item{y}{
a vector similar to \code{x}, of the same length and in the same order.
}
  \item{method}{
the similarity index to use. Available options are \code{'Baroni'} and \code{'Jaccard'}.
}
}
\details{
}
\value{
The value of fuzzy similarity between \code{x} and \code{y}.
}
\references{
Baroni-Urbani, C. & Buser, M.W. (1976) Similarity of Binary Data. Systematic Zoology, 25: 251-259.

Jaccard, P. (1901) Etude comparative de la distribution florale dans une portion des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 37: 547-579.

Zadeh L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353. 
}
\author{
A. Marcia Barbosa
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{simMat}}
}
\examples{
data(rotifers01)

head(rotifers01)

names(rotifers01)

fuzSim(rotifers01[,6], rotifers01[,7], method = "Baroni")
}
\keyword{ similarity }
\keyword{ fuzzy }
