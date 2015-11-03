\name{modOverlap}
\alias{modOverlap}
\title{Overall overlap between model predictions}
\description{
This function calculates the degree of overlap between the predictions of two models, using niche comparison metrics such as Schoener's D, Hellinger distance and Warren's I.}
\usage{
modOverlap(pred1, pred2, na.rm = TRUE)
}
\arguments{
  \item{pred1}{numeric vector of the predictions of a generalized linear model (values between 0 and 1).}
  \item{pred2}{numeric vector of the predictions of another generalized linear model; must be of the same length and in the same order as \code{pred1}.}
  \item{na.rm}{logical value indicating whether \code{NA} values should be removed prior to calculation.}
}
\details{
See Warren et al. (2008).
}
\value{
This function returns a list of 3 metrics:
\item{SchoenerD}{Schoener's (1968) D statistic for niche overlap, varying between 0 (no overlap) and 1 (identical niches).}
\item{WarrenI}{the I index of Warren et al. (2008), based on Hellinger distance (below) but re-formulated to also vary between 0 (no overlap) and 1 (identical niches).}
\item{HellingerDist}{Hellinger distance (as in van der Vaart 1998, p. 211) between probability distributions, varying between 0 and 2.}
}
\references{
Schoener T.W. (1968) Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49: 704–726

van der Vaart A.W. (1998)  Asymptotic statistics. Cambridge Univ. Press, Cambridge (UK)

Warren D.L., Glor R.E. & Turelli M. (2008) Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. Evolution, 62: 2868–83 (and further ERRATUM)
}
\author{A. Marcia Barbosa}
\note{
A function providing similar measures, \code{niche.overlap}, is available in package \pkg{phyloclim}, but it requires complex and software-specific input data formats.
}
\seealso{
\code{\link{fuzSim}}; \code{niche.overlap} in package \pkg{phyloclim}
}
\examples{
}
\keyword{  }
