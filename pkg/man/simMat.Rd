\name{simMat}
\alias{simMat}
\title{
Pair-wise (fuzzy) similarity matrix
}
\description{
\code{simMat} takes multiple species occurrence data or regional species composition, either categorical (0 or 1) or fuzzy (between 0 and 1), and uses the \code{\link{fuzSim}} function to compute a square matrix of pair-wise similarities between them, using a fuzzy logic version (Barbosa, 2015) of the specified similarity index.
}
\usage{
simMat(data, method, diag = TRUE, upper = TRUE, verbosity = 2, plot = FALSE, ...)
}
\arguments{
  \item{data}{
a matrix, data frame, or multilayer SpatRaster containing (optionally fuzzy) species presence-absence data (in wide format, i.e. one column or layer per species), with 1 meaning presence, 0 meaning absence, and values in between for fuzzy presence (or the degree to which each locality belongs to the set of species presences; see Zadeh, 1965). Fuzzy presence-absence can be obtained, for example, with \code{\link{multGLM}}, \code{\link{distPres}} or \code{\link{multTSA}}. These data can also be \code{\link{transpose}}d for comparing regional species compositions.
}
  \item{method}{
the similarity index whose fuzzy version to use. See \code{\link{fuzSim}} for available options.
}
  \item{diag}{
logical value indicating whether the diagonal of the matrix should be filled (with ones). Defaults to TRUE.
}
  \item{upper}{
logical value indicating whether the upper triangle of the matrix (symmetric to the lower triangle) should be filled. Defaults to TRUE.
}
  \item{verbosity}{
integer value indicating the amount of messages to display; currently meaningful values are 0, 1, and 2 (the default).
}
  \item{plot}{logical argument indicating whether to also plot the matrix as an image. The default is FALSE (for back-compatibility).}
  \item{\dots}{some additional arguments can be passed to \code{\link{image}} (and through to \code{\link{plot}}) if plot=TRUE, such as 'col', 'main', 'font.main' or 'cex.main' (not 'axes', 'xlab' or 'ylab', which are already defined by simMat).}
}
\details{
The fuzzy versions of species occurrence data and of binary similarity indices introduce tolerance for small spatial differences in species' occurrence localities, allow for uncertainty about species occurrence, and may compensate for under-sampling and geo-referencing errors (Barbosa, 2015).
}
\value{
This function returns a square matrix of pair-wise similarities among the species distributions (columns) in \code{data}. Similarity is calculated with the fuzzy version of the index specified in \code{method}, which yields traditional binary similarity if the data are binary (0 or 1), or fuzzy similarity if the data are fuzzy (between 0 and 1) (Barbosa, 2015).
}
\references{
Barbosa A.M. (2015) fuzzySim: applying fuzzy logic to binary similarity indices in ecology. Methods in Ecology and Evolution, 6: 853-858.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{fuzSim}}
}
\examples{

# load and look at the rotif.env presence-absence data:

data(rotif.env)

head(rotif.env)

names(rotif.env)


# build a matrix of similarity among these binary data
# using e.g. Jaccard's index:

bin.sim.mat <- simMat(rotif.env[ , 18:47], method = "Jaccard")

head(bin.sim.mat)


# calculate a fuzzy version of the presence-absence data
# based on inverse distance to presences:

rotifers.invd <- distPres(rotif.env, sp.cols = 18:47,
coord.cols = c("Longitude", "Latitude"), id.col = 1, suffix = ".d",
p = 1, inv = TRUE)

head(rotifers.invd)


# build a matrix of fuzzy similarity among these fuzzy
# distribution data, using the fuzzy version of Jaccard's index:

fuz.sim.mat <- simMat(rotifers.invd[ , -1], method = "Jaccard")

head(fuz.sim.mat)


# plot the similarity matrices as colours:

image(x = 1:ncol(bin.sim.mat), y = 1:nrow(bin.sim.mat),
z = bin.sim.mat, col = rev(heat.colors(256)), xlab = "", ylab = "",
axes = FALSE, main = "Binary similarity")
axis(side = 1, at = 1:ncol(bin.sim.mat), tick = FALSE,
labels = colnames(bin.sim.mat), las = 2)
axis(side = 2, at = 1:nrow(bin.sim.mat), tick = FALSE,
labels = rownames(bin.sim.mat), las = 2)

image(x = 1:ncol(fuz.sim.mat), y = 1:nrow(fuz.sim.mat),
z = fuz.sim.mat, col = rev(heat.colors(256)), xlab = "", ylab = "",
axes = FALSE, main = "Fuzzy similarity")
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
# (or fuzzy biotic regions if you transpose 'data'),
# so that localities are in columns and species in rows)
# using the RMACOQUI package (Olivero et al. 2011)
}
