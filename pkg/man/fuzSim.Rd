\name{fuzSim}
\alias{fuzSim}
\title{
Fuzzy similarity
}
\description{
Fuzzy similarity, based on a fuzzy version of the binary similarity index 
specified in \code{method}, between the presence-absence data of two species 
(or between two other binary variables).
}
\usage{
fuzSim(x, y, method)
}
\arguments{
  \item{x}{
a vector of (optionally fuzzy) species presence-absence data, with 1 meaning 
presence, 0 meaning absence, and values in between meaning fuzzy presence 
(or the degree to which the locality belongs to the set of species presences; 
Zadeh, 1965). Fuzzy presence-absence can be obtained, for example, with 
functions \code{\link{distPres}} or \code{\link{multTSA}} in this package.
}
  \item{y}{
a vector similar to \code{x}, of the same length and in the same order.
}
  \item{method}{
the similarity index to use. Currently available options are \code{'Baroni'}, \code{'Jaccard'} and \code{'Sorensen'}.
}
}
\details{
Similarity between ecological communities, beta diversity patterns, biotic 
regions and distributional relationships among species are commonly determined 
based on pair-wise similarities in species' occurrence patterns. Some of the 
most commonly employed pair-wise similarity indices are those of Jaccard (1901), 
Baroni-Urbani and Buser (1976) and Sorensen (1948), which are here implemented 
in their fuzzy versions (Barbosa, submitted), which are able to deal with both 
binary and fuzzy data.
}
\value{
The value of fuzzy similarity between \code{x} and \code{y}.
}
\references{
Baroni-Urbani C. & Buser M.W. (1976) Similarity of Binary Data. Systematic 
Zoology, 25: 251-259.

Jaccard P. (1901) Etude comparative de la distribution florale dans une portion 
des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 
37: 547-579.

Sorensen T. (1948) A method of establishing groups of equal amplitude in plant 
sociology based on similarity of species and its application to analyses of the 
vegetation on Danish commons. Kongelige Danske Videnskabernes Selskab 5(4): 1-34

Zadeh L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353. 
}
\author{
A. Marcia Barbosa
}
\note{
}
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
