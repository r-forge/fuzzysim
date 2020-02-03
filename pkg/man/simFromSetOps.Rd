\name{simFromSetOps}
\alias{simFromSetOps}
\title{
Calculate similarity from set operations
}
\description{
This function calculates pair-wise similarity based on the results of set operations (intersection, union) among the subjects.
}
\usage{
simFromSetOps(size1, size2, intersection, union, total.size = NULL, 
method = c("Jaccard", "Sorensen", "Simpson", "Baroni"), 
verbosity = 1)
}
\arguments{
  \item{size1}{
size of subject 1 (e.g., area of the distribution range of a species, or its number of presences within a grid). Not needed if method = "Jaccard".
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
total size of the study area. Needed only when calculating a similarity index that takes shared absences into account (i.e., method = "Baroni").
}
  \item{method}{
the similarity index to use. Currently implemented options are "Jaccard", "Sorensen", "Simpson" or "Baroni".
}
  \item{verbosity}{
integer indicating whether to display messages.
}
}
\details{
Similarities among ecological communities, beta diversity patterns, biotic regions, and distributional relationships among species are commonly determined based on pair-wise (dis)similarities in species' occurrence patterns. This function implements some of the most commonly employed similarity indices, namely those of Jaccard (1901), Sorensen (1948), Simpson (1960) and Baroni-Urbani & Buser (1976), based on the amount of occupied and overlap area between two species.
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
\seealso{
\code{\link{fuzSim}}, \code{\link{simMat}}
}
\examples{
# take two species which occur in 22 and 35 area units, respectively
# and which overlap in 8 of those units:

sp1 <- 22
sp2 <- 35
int <- 8
uni <- sp1 + sp2 - int


# calculate similarity between their distributions based on 
# different indices:

simFromSetOps(intersection = int, union = uni, method = "Jaccard")

simFromSetOps(sp1, sp2, int, uni, method = "Sorensen")

simFromSetOps(sp1, sp2, int, uni, method = "Simpson")


# if you want Baroni-Urbani & Buser's index
# you need to provide also the total size of your study area:

simFromSetOps(sp1, sp2, int, uni, total = 100, method = "Baroni")
}
