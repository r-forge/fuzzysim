\name{fuzSim}
\alias{fuzSim}
\title{
Fuzzy similarity
}
\description{
Fuzzy similarity, based on a fuzzy version of the binary similarity index 
specified in \code{method}, between two binary or fuzzy variables.
}
\usage{
fuzSim(x, y, method)
}
\arguments{
  \item{x}{
a vector of (optionally fuzzy) presence-absence data, with 1 meaning 
presence, 0 meaning absence, and values in between meaning fuzzy presence 
(or the degree to which each locality belongs to the set of species presences, 
or to which each species belongs to the locality; Zadeh, 1965). Fuzzy 
presence-absence can be obtained, for example, with functions \code{\link{distPres}} 
or \code{\link{multTSA}} in this package.
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
regions, and distributional relationships among species are commonly determined 
based on pair-wise similarities in species' occurrence patterns. Some of the 
most commonly employed pair-wise similarity indices are those of Jaccard (1901), 
Sorensen (1948) and Baroni-Urbani & Buser (1976), which are here implemented 
in their fuzzy versions (Barbosa, submitted), able to deal with both binary and 
fuzzy data.
}
\value{
The value of fuzzy similarity between \code{x} and \code{y}.
}
\note{
The original formula of Baroni-Urbani & Buser (1976) has been replaced with a 
mathematically equivalent, slightly different-looking but more efficient one 
(adapted from Olivero et al. 1998). Not only the letters are switched, but in 
Baroni's formula the B and C were the numbers of attributes present in one 
element but not in the other one, while here the A and B are the numbers of 
attributes present in each element, whether or not they are also present in 
the other one. This makes the formula faster to calculate, visibly for large 
datasets, and more coherent with Jaccard's and Sorensen's indices.
}
\references{
Baroni-Urbani C. & Buser M.W. (1976) Similarity of Binary Data. Systematic 
Zoology, 25: 251-259

Jaccard P. (1901) Etude comparative de la distribution florale dans une portion 
des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 
37: 547-579

Olivero J., Real R. & Vargas J.M. (1998) Distribution of breeding, wintering 
and resident waterbirds in Europe: biotic regions and the macroclimate. Ornis 
Fennica, 75: 153-175

Sorensen T. (1948) A method of establishing groups of equal amplitude in plant 
sociology based on similarity of species and its application to analyses of the 
vegetation on Danish commons. Kongelige Danske Videnskabernes Selskab, 5(4): 1-34

Zadeh L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353
}
\author{
A. Marcia Barbosa
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
