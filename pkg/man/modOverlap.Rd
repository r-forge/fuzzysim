\name{modOverlap}
\alias{modOverlap}
\title{Overall overlap between model predictions}
\description{
This function calculates the degree of overlap between the predictions of two models, using niche comparison metrics such as Schoener's D, Hellinger distance and Warren's I.}
\usage{
modOverlap(pred1, pred2, na.rm = TRUE)
}
\arguments{
  \item{pred1}{numeric vector or SpatRaster layer of the predictions of a model, with values between 0 and 1.}
  \item{pred2}{numeric vector or SpatRaster layer of the predictions of another model, also with values between 0 and 1; must be of the same dimensions and in the same order as 'pred1'.}
  \item{na.rm}{logical value indicating whether NA values should be removed prior to calculation. The default is TRUE.}
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
Schoener T.W. (1968) Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49: 704-726

van der Vaart A.W. (1998)  Asymptotic statistics. Cambridge Univ. Press, Cambridge (UK)

Warren D.L., Glor R.E. & Turelli M. (2008) Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. Evolution, 62: 2868-83 (and further ERRATUM)
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{fuzSim}}; \code{\link{fuzzyOverlay}}; \code{niche.overlap} in package \pkg{phyloclim}; \code{ecospat.niche.overlap} in package \pkg{ecospat}
}
\examples{
# get an environmental favourability model for a rotifer species:

data(rotif.env)

names(rotif.env)

fav_current <- multGLM(rotif.env, sp.cols = 18, var.cols = 5:17,
step = TRUE, FDR = TRUE, trim = TRUE, P = FALSE, Fav = TRUE) $
predictions


# imagine you have a model prediction for this species in a future time
# (here we will create one by randomly jittering the current predictions)

fav_imag <- jitter(fav_current, amount = 0.2)
fav_imag[fav_imag < 0] <- 0
fav_imag[fav_imag > 1] <- 1


# calculate niche overlap between current and imaginary future predictions:

modOverlap(fav_current, fav_imag)
}
