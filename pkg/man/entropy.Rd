\name{entropy}
\alias{entropy}
\title{
(Fuzzy) entropy
}
\description{
This function computes fuzzy entropy (Kosko 1986, Estrada & Real 2021), or optionally Shannon's (1948) entropy.
}
\usage{
entropy(data, sp.cols = 1:ncol(data), method = "fuzzy", base = exp(1),
plot = TRUE, plot.type = "lollipop", na.rm = TRUE, ...)
}
\arguments{
  \item{data}{a vector, matrix or data frame containing the data to analyse.}
  \item{sp.cols}{names or index numbers of the columns of 'data' that contain the values for which to compute entropy (if 'data' is not a vector). The default is to use all columns.}
  \item{method}{character value indicating the method to use. Can be "fuzzy" (the default) or "Shannon". The former requires the input to be a fuzzy system (e.g. \code{\link{Fav}}ourability values), while the latter requires probabilities. If method="Shannon" and the values for a vector or column do not sum up to 1, they are divided by their sum so that this additional requirement is met (Estrada & Real 2021).}
  \item{base}{base for computing the logarithm if method="Shannon". The default is the natural logarithm.}
  \item{plot}{logical value indicating whether to plot the results (if 'data' has more than one column). The default is TRUE.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "lollipop" (the default) or "barplot".}
  \item{na.rm}{logical value indicating whether NA values should be removed before computations. The default is TRUE.}
  \item{\dots}{additional arguments to be passed to \code{barplot} or to \code{modEvA::lollipop}.}
}
\details{
Fuzzy entropy (Kosko 1986) applies to fuzzy systems (such as \code{\link{Fav}}ourability) and it can take values between zero and one. Fuzzy entropy equals one when the distribution of the values is uniform, i.e. 0.5 in all localities. The smaller the entropy, the more orderly the distribution of the values, i.e. the closer they are to 0 or 1, distinguishing (potential) presences and absences more clearly. Fuzzy entropy can reflect the overall degree of uncertainty in a species' distribution model predictions, and it is directly comparable across species and study areas (Estrada & Real 2021).

Shannon's entropy requires that the input values are probabilities and sum up to 1 (Shannon 1948). This makes sense when analysing the probability that a unique event occurs in a finite universe. However, if a species has more than one presence, the sum of probabilities in all localities equals the number of presences. To satisfy the condition that the inputs sum up to 1, this function divides each value by the sum of values when this is not the case (if method="Shannon"). Notice that this has a mathematical justification but not a biogeographical sense, and (unlike fuzzy entropy) the results are comparable only between models based on the same number of presences + absences, e.g. in a context of selection of variables for a model (Estrada & Real 2021).
}
\value{
This function returns a numeric value of entropy for 'data' (if it is a numeric vector) or for each of 'sp.cols' in 'data' (if it is a matrix or data frame). Optionally (and by default), a plot is also produced with these values (if there is more than one column) for visual comparison.
}
\references{
Estrada A. & Real R. (2021) A stepwise assessment of parsimony and fuzzy entropy in species distribution modelling. Entropy, 23: 1014

Kosko B. (1986) Fuzzy entropy and conditioning. Information Sciences, 40: 165-174

Shannon C.E. (1948) A mathematical theory of communication. Bell System Technical Journal, 27: 379-423
}
\author{A. Marcia Barbosa}
\examples{
data(rotif.env)

pred <- multGLM(rotif.env, sp.cols = 18:20, var.cols = 5:17)$predictions

head(pred)

entropy(pred, sp.cols = c("Abrigh_F", "Afissa_F", "Apriod_F"))

entropy(pred, sp.cols = c("Abrigh_P", "Afissa_P", "Apriod_P"), method = "Shannon")
}
