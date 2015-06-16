\name{simFromSetOps}
\alias{simFromSetOps}
\title{
Calculate similarity from set operations
}
\description{
This function calculates pair-wise similarity based on the results of set operations (intersection, union) among the subjects.
}
\usage{
simFromSetOps(size1, size2, intersection, union, total.size = NULL, method = c("Jaccard", "Sorensen", "Simpson", "Baroni"))
}
\arguments{
  \item{size1}{
size of subject 1 (e.g., area of the distribution range of a species, or its number of presences within a grid).
}
  \item{size2}{
the same for subject 2.
}
  \item{intersection}{
size of the intersection among subjects 1 and 2 (area of the intersection among their distribution ranges, or number of grid cells in which they co-occur).
}
  \item{union}{
size of the union of subjects 1 and 2.
}
  \item{total.size}{
total size of the study area; necessary only when calculating similarity indices which take double absences into account (e.g. Baroni).
}
  \item{method}{
the similarity index to use. Currently available options are \code{'Jaccard'}, \code{'Sorensen'}, \code{'Simpson'} and \code{'Baroni'} (see Details in \code{\link{fuzSim}}).
}
}
\details{
}
\value{
The numeric value of similarity among subjects 1 and 2.
}
\references{
Baroni-Urbani C. & Buser M.W. (1976) Similarity of Binary Data. Systematic Zoology, 25: 251-259

Jaccard P. (1901) Etude comparative de la distribution florale dans une portion des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 37: 547-579

Simpson, G.G. (1960) Notes on the measurement of faunal resemblance. Amer. J. Sci. 258A, 300-311

Sorensen T. (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species and its application to analyses of the vegetation on Danish commons. Kongelige Danske Videnskabernes Selskab, 5(4): 1-34
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{fuzSim}}, \code{\link{simMat}}
}
\examples{
# take two species whose distribution ranges have 22 and 35 square kilometers, respectively; and which overlap in 8.5 square kilometers:

sp1 <- 22
sp2 <- 35
int <- 8.5
uni <- sp1 + sp2 - int

simFromSetOps(sp1, sp2, int, uni, method = "Jaccard")


# if you want Baroni-Urbani & Buser's index, you need to provide also the total size of your study area:

simFromSetOps(sp1, sp2, int, uni, total = 100, method = "Baroni")
}
\keyword{}
