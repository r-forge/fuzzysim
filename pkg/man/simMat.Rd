\name{simMat}
\alias{simMat}
\title{
Pair-wise (fuzzy) similarity matrix
}
\description{
\code{simMat} takes a matrix or data frame containing species occurrence data, 
either categorical (0 or 1) or fuzzy (between 0 and 1), and uses the 
\code{\link{fuzSim}} function to calculate a square matrix of pair-wise 
similarities between them, using a fuzzy logic version (after Zadeh, 1965) of 
either Jaccard's (1901), Baroni-Urbani & Buser's (1976) or Sorensen's (1948) 
binary similarity index.
}
\usage{
simMat(data, method)
}
\arguments{
  \item{data}{
a matrix or data frame containing (optionally fuzzy) species presence-absence 
data (one column per species), with 1 meaning presence, 0 meaning absence, and 
values in between for fuzzy presence (or the degree to which each locality 
belongs to the set of species presences; see Zadeh, 1965). Fuzzy presence
-absence can be obtained, for example, with \code{\link{distPres}} or with 
\code{\link{multTSA}}.
}
  \item{method}{
the similarity index whose fuzzy version to use. Available options are \code{'Baroni'}, \code{'Jaccard'} and \code{'Sorensen'}.
}
}
\details{
Spatial associations between species' distributions can provide deep insights 
into the processes that drive biodiversity patterns. Chorological clustering 
provides a systematic framework for analysing such associations, by detecting 
areas with similar species composition (biotic regions) or clusters of species' 
distribution types (chorotypes; Olivero et al. 2011, 2013). The fuzzy versions 
of species occurrence data and of the similarity indices introduce tolerance 
for small spatial differences in species' occurrence localities and may also 
compensate geo-referencing errors. The results of \code{simMat} can be used 
for determining chorotypes or biotic regions using the \code{RMACOQUI} package 
(Olivero et al. 2011).
}
\value{
A square matrix of pair-wise similarity among the species distributions 
(columns) in \code{data}. Similarity is calculated with the fuzzy version of 
the index specified in \code{method}, which yields traditional binary 
similarity if the data are binary (0 or 1), or fuzzy similarity if the data 
are fuzzy (between 0 and 1).
}
\references{
Baroni-Urbani, C. & Buser, M.W. (1976) Similarity of Binary Data. Systematic 
Zoology, 25: 251-259.

Jaccard, P. (1901) Etude comparative de la distribution florale dans une portion 
des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 
37: 547-579.

Olivero, J., Real, R. & Marquez, A.L. (2011) Fuzzy chorotypes as a conceptual 
tool to improve insight into biogeographic patterns. Systematic Biology, 
60: 645-60.

Olivero, J., Marquez, A.L., Real, R. (2013) Integrating fuzzy logic and 
statistics to improve the reliable delimitation of biogeographic regions 
and transition zones. Systematic Biology, 62: 1-21.

Sorensen T. (1948) A method of establishing groups of equal amplitude in 
plant sociology based on similarity of species and its application to 
analyses of the vegetation on Danish commons. Kongelige Danske Videnskabernes 
Selskab 5(4): 1-34

Zadeh L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353. 
}
\author{
A. Marcia Barbosa
}
\note{
}

\seealso{
\code{\link{fuzSim}}
}
\examples{

# load and look at the rotifers01 presence-absence data:

data(rotifers01)

head(rotifers01)

names(rotifers01)


# build a matrix of similarity among these binary data 
# using e.g. Baroni-Urbani & Buser's index:

bin.sim.mat <- simMat(rotifers01[ , 6:40], method = "Baroni")

head(bin.sim.mat)


# calculate a fuzzy version of the presence-absence data 
# based on inverse distance to presences:

rotifers.invd <- distPres(rotifers01, sp.cols = 6:40, 
coord.cols = c("Long", "Lati"), id.col = 1, suffix = ".d", p = 1, inv = TRUE)

head(rotifers.invd)


# build a matrix of fuzzy similarity among these fuzzy distribution data, 
# with the fuzzy version of Baroni-Urbani & Buser's index:

fuz.sim.mat <- simMat(rotifers.invd[ , -1], method = "Baroni")

head(fuz.sim.mat)


# plot the similarity matrices as colours:

image(x = 1:ncol(bin.sim.mat), y = 1:nrow(bin.sim.mat), z = bin.sim.mat, 
col = rev(heat.colors(256)), xlab = "", ylab = "", axes = FALSE, 
main = "Binary similarity")
axis(side = 1, at = 1:ncol(bin.sim.mat), tick = FALSE, 
labels = colnames(bin.sim.mat), las = 2)
axis(side = 2, at = 1:nrow(bin.sim.mat), tick = FALSE, 
labels = rownames(bin.sim.mat), las = 2)

image(x = 1:ncol(fuz.sim.mat), y = 1:nrow(fuz.sim.mat), z = fuz.sim.mat, 
col = rev(heat.colors(256)), xlab = "", ylab = "", axes = FALSE, 
main = "Fuzzy similarity")
axis(side = 1, at = 1:ncol(fuz.sim.mat), tick = FALSE, 
labels = colnames(fuz.sim.mat), las = 2, cex = 0.5)
axis(side = 2, at = 1:nrow(fuz.sim.mat), tick = FALSE, 
labels = rownames(fuz.sim.mat), las = 2)


# plot a UPGMA dendrogram from each similarity matrix:

plot(hclust(as.dist(1 - bin.sim.mat), method = "average"), 
main = "Binary cluster dendrogram")

plot(hclust(as.dist(1 - fuz.sim.mat), method = "average"), 
main = "Fuzzy cluster dendrogram")


# you can get fuzzy chorotypes from these similarity matrices 
# (or fuzzy biotic regions if your transpose \code{data} so that localities 
# are in columns and species in rows) using the \code{RMACOQUI} package 
# (Olivero et al. 2011)
}
\keyword{ similarity }
\keyword{ fuzzy }