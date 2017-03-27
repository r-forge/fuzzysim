\name{multTSA}
\alias{multTSA}
\title{
Trend Surface Analysis for multiple species
}
\description{
This function performs trend surface analysis for multiple species at a time. It converts categorical presence-absence (1-0) data into continuous surfaces denoting the spatial trend in species' occurrence patterns.
}
\usage{
multTSA(data, sp.cols, coord.cols, id.col = NULL, degree = 3, 
step = TRUE, Favourability = FALSE, suffix = "_TS", 
save.models = FALSE)
}
\arguments{
  \item{data}{
a matrix or data frame containing, at least, two columns with spatial coordinates, and one column per species containing their presence (1) and absence (0) data, with localities in rows.
}
  \item{sp.cols}{
names or index numbers of the columns containing the species presences and absences in data. Must contain only zeros (0) for absences and ones (1) for presences.
}
  \item{coord.cols}{
names or index numbers of the columns containing the spatial coordinates in data (x and y, or longitude and latitude, in this order!).
}
  \item{id.col}{
optionally, the name or index number of a column (to be included in the output) containing locality identifiers in data.
}
  \item{degree}{
the degree of the spatial polynomial to use (see Details). The default is 3.
}
  \item{step}{
logical value indicating whether the regression of presence-absence on the spatial polynomial should do a stepwise inclusion of the polynomial terms (using the \code{\link{step}} function with default settings, namely backward AIC selection), rather than forcing all terms into the equation. The default is \code{TRUE}.
}
  \item{Favourability}{
logical value indicating whether the probability values obtained from the regression should be converted to favourability, so that they are more directly comparable among species with different prevalence (see Real et al. 2006, Acevedo & Real 2012). The default is \code{FALSE}.
}
  \item{suffix}{
character indicating the suffix to add to the trend surface columns in the resulting data frame. The default is \code{"_TS"}.
}
  \item{save.models}{
logical value indicating whether the models obtained from the regressions should be saved in the results. The default is \code{FALSE}.
}
}
\details{
Trend Surface Analysis is a way to model the spatial structure in species' distributions by regressing occurrence data on the spatial coordinates x and y, for a linear trend, or on polynomial terms of these coordinates (x^2, y^2, x*y, etc.), for curvilinear trends (Legendre & Legendre, 1998; Borcard et al., 2011). Second- and third-degree polynomials are often used. \code{multTSA} allows specifying the degree of the spatial polynomial to use. By default, it uses a 3rd-degree polynomial and performs stepwise AIC selection of the polynomial terms to include.
}
\value{
This function returns a matrix or data frame containing the identifier column (if provided in \code{id.col}) and one column per species containing the value predicted by the trend surface analysis.
}
\references{
Acevedo P. & Real R. (2012) Favourability: concept, distinctive characteristics and potential usefulness. Naturwissenschaften 99: 515-522

Borcard D., Gillet F. & Legendre P. (2011) Numerical Ecology with R. Springer, New York.

Legendre P. & Legendre L. (1998) Numerical Ecology. Elsevier, Amsterdam.

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{distPres}}, \code{\link{poly}}
}
\examples{
data(rotif.env)

head(rotif.env)

names(rotif.env)

tsa <- multTSA(rotif.env, sp.cols = 18:20, 
coord.cols = c("Longitude", "Latitude"), id.col = 1)

head(tsa)
}
\keyword{ spatial }
\keyword{ multivariate }
\keyword{ regression }
\keyword{ models }
