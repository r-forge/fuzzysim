\name{fuzSim}
\alias{fuzSim}
\title{
Fuzzy similarity
}
\description{
This function calculates fuzzy similarity, based on a fuzzy version of the binary similarity index specified in \code{method}, between two binary (0 or 1) or fuzzy (between 0 and 1) variables.
}
\usage{
fuzSim(x, y, method, na.rm = TRUE)
}
\arguments{
  \item{x}{
numeric vector or SpatRaster layer of (optionally fuzzy) presence-absence data, with 1 meaning presence, 0 meaning absence, and values in between meaning fuzzy presence (or the degree to which each locality belongs to the set of species presences, or to which each species belongs to the locality; Zadeh, 1965). Fuzzy presence-absence can be obtained, for example, with functions \code{\link{multGLM}}, \code{\link{multTSA}} or \code{\link{distPres}} in this package, or with any other function that computes presence probability or spatial/environmental suitability for a species.
}
  \item{y}{
numeric vector or SpatRaster to overlap with 'x', of the same length and in the same order.
}
  \item{method}{
the similarity index to compute between x and y. Currently available options are "Jaccard", "Sorensen", "Simpson" and "Baroni" (see Details).
}
  \item{na.rm}{
logical value indicating whether NA values should be ignored. The default is TRUE.
}
}
\details{
Similarity between ecological communities, beta diversity patterns, biotic regions, and distributional relationships among species are commonly determined based on pair-wise (dis)similarities in species' occurrence patterns. Some of the most commonly employed similarity indices are those of Jaccard (1901), Sorensen (1948), Simpson (1960) and Baroni-Urbani & Buser (1976), which are here implemented in their fuzzy versions (Barbosa, 2015), able to deal with both binary and fuzzy data. Jaccard's and Baroni's indices have associated tables of significant values (Baroni-Urbani & Buser 1976, Real & Vargas 1996, Real 1999).

Note that the Jaccard index's translation to fuzzy logic (where intersection = minimum and union = maximum) is equivalent to the weighted Jaccard index (Ioffe 2010) and to the overlap, coincidence and consistence indices of Real et al. (2010).

Jaccard's and Sorensen's indices have also been recommended as prevalence-independent metrics for evaluating the performance of models of species distributions and ecological niches (Leroy et al. 2018). These indices are equivalent to other previously recommended model evaluation metrics: the F-measure (which equals Sorensen's index), and the proxy of the F-measure for presence-background data, which equals 2 times Jaccard's index (Li and Guo 2013, Leroy et al. 2018).

}
\value{
The function returns a value between 0 and 1 representing the fuzzy similarity between the provided 'x' and 'y' vectors. Note, for example, that Jaccard similarity can be converted to dissimilarity (or Jaccard distance) if subtracted from 1, while \code{1-Sorensen} is not a proper distance metric as it lacks the property of triangle inequality (see \url{https://en.wikipedia.org/wiki/S\%C3\%B8rensen\%E2\%80\%93Dice_coefficient}).
}
\note{
The formulas used in this function may look slighty different from some of their published versions (e.g. Baroni-Urbani & Buser 1976), not only because the letters are switched, but because here the A and B are the numbers of attributes present in each element, whether or not they are also present in the other one. Thus, our 'A+B' is equivalent to 'A+B+C' in formulas where A and B are the numbers of attributes present in one but not the other element, and our A+B-C is equivalent to their A+B+C. The formulas used here (adapted from Olivero et al. 1998) are faster to calculate, visibly for large datasets.
}
\references{
Barbosa A.M. (2015) fuzzySim: applying fuzzy logic to binary similarity indices in ecology. Methods in Ecology and Evolution, 6: 853-858.

Baroni-Urbani C. & Buser M.W. (1976) Similarity of Binary Data. Systematic Zoology, 25: 251-259

Ioffe S. (2010) Improved Consistent Sampling, Weighted Minhash and L1 Sketching. 2010 IEEE International Conference on Data Mining, Sydney, NSW, Australia, pp. 246-255, doi: 10.1109/ICDM.2010.80

Jaccard P. (1901) Etude comparative de la distribution florale dans une portion des Alpes et des Jura. Memoires de la Societe Vaudoise des Sciences Naturelles, 37: 547-579

Leroy B., Delsol R., Hugueny B., Meynard C. N., Barhoumi C., Barbet-Massin M. & Bellard C. (2018) Without quality presence-absence data, discrimination metrics such as TSS can be misleading measures of model performance. Journal of Biogeography, 45: 1994-2002

Li W. & Guo Q. (2013) How to assess the prediction accuracy of species presence-absence models without absence data? Ecography, 36: 788-799

Olivero J., Real R. & Vargas J.M. (1998) Distribution of breeding, wintering and resident waterbirds in Europe: biotic regions and the macroclimate. Ornis Fennica, 75: 153-175

Real R. (1999) Tables of significant values of Jaccard's index of similarity. Miscellania Zoologica 22: 29:40

Real R. & Vargas J.M (1996) The probabilistic basis of Jaccard's index of similarity. Systematic Biology 45: 380-385

Simpson, G.G. (1960) Notes on the measurement of faunal resemblance. Amer. J. Sci. 258A, 300-311

Sorensen T. (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species and its application to analyses of the vegetation on Danish commons. Kongelige Danske Videnskabernes Selskab, 5(4): 1-34

Zadeh L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{simMat}}; \code{\link{modOverlap}}
}
\examples{
data(rotif.env)

names(rotif.env)


# you can calculate similarity between binary species occurrence patterns:

fuzSim(rotif.env[, "Abrigh"], rotif.env[, "Afissa"], method = "Jaccard")
fuzSim(rotif.env[, "Abrigh"], rotif.env[, "Afissa"], method = "Sorensen")
fuzSim(rotif.env[, "Abrigh"], rotif.env[, "Afissa"], method = "Simpson")
fuzSim(rotif.env[, "Abrigh"], rotif.env[, "Afissa"], method = "Baroni")


# or you can model environmental favourability for these species
# and calculate fuzzy similarity between their environmental predictions
# which goes beyond the strict coincidence of their occurrence records:

fav <- multGLM(rotif.env, sp.cols = 18:19, var.cols = 5:17, step = TRUE,
FDR = TRUE, trim = TRUE, P = FALSE, Fav = TRUE) $ predictions

fuzSim(fav[, "Abrigh_F"], fav[, "Afissa_F"], method = "Jaccard")
fuzSim(fav[, "Abrigh_F"], fav[, "Afissa_F"], method = "Sorensen")
fuzSim(fav[, "Abrigh_F"], fav[, "Afissa_F"], method = "Simpson")
fuzSim(fav[, "Abrigh_F"], fav[, "Afissa_F"], method = "Baroni")
}
