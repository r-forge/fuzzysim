\name{multTSA}
\alias{multTSA}
\title{
Trend Surface Analysis for multiple species
}
\description{
This function performs trend surface analysis for one or more species at a time. It converts categorical presence-(pseudo)absence (1-0) data into continuous surfaces denoting the spatial trend in species' occurrence patterns.
}
\usage{
multTSA(data, sp.cols, coord.cols, id.col = NULL, degree = 3,
step = TRUE, criterion = "AIC", type = "P", Favourability = FALSE,
suffix = "_TS", save.models = FALSE, na.rm = TRUE, verbosity = 2, ...)
}
\arguments{
  \item{data}{
a matrix or an object inheriting class data frame containing, at least, two columns with spatial coordinates, and one column per species containing their presence (1) and absence (0) data, with localities in rows. Alternatively, a terra SpatRaster map over whose pixels a spatial trend is to be computed.
}
  \item{sp.cols}{
names or index numbers of the columns containing the species presences and absences in 'data', containing only zeros (0) for absences and ones (1) for presences. Or, if 'data' is a SpatRaster, an object inheriting class 'data.frame' with two columns containing the point coordinates of the presences (x and y, or longitude and latitude, in this order!). For SpatRaster inputs, the function is currently only implemented for one species at a time.
}
  \item{coord.cols}{
names or index numbers of the columns containing the spatial coordinates in data (x and y, or longitude and latitude, in this order!). Ignored if 'data' is a SpatRaster and 'sp.cols' is a table of spatial coordinates.
}
  \item{id.col}{
optional (default NULL) name or index number of a column (to be included in the output) containing locality identifiers in 'data'. Ignored if 'data' is a SpatRaster.
}
  \item{degree}{
the degree of the spatial polynomial to use (see Details). The default is 3.
}
  \item{step}{
logical value indicating whether the regression of presence-absence on the spatial polynomial should do a stepwise inclusion of the polynomial terms (using the \code{\link{step}} function with default settings, namely backward AIC selection), rather than forcing all terms into the equation. The default is TRUE.
}
  \item{criterion}{
character value indicating whether the backward stepwise selection of variables (if step = TRUE) should be made according to "AIC" (the default, using the \code{\link{step}} function) or to "significance" (using the \code{\link{modelTrim}} function).
}
  \item{type}{
the type of trend surface to obtain. Can be either "Y" for the raw polynomial equation (i.e. in the scale of the predictors, e.g. if you want to use the spatial trend as a predictor variable in a model), "P" for the logit-transformed probability (e.g. if you want to use the output as a prediction of presence probability based on spatial trend alone), or "F" for spatial favourability, i.e., prevalence-independent probability (see \code{\link{Fav}}).
}
  \item{Favourability}{
deprecated argument; 'type' should now be used instead, although (at least for the timebeing) this will still be accepted (with Favourability=TRUE internally resulting in type="F") for back-compatibility.
}
  \item{suffix}{
character indicating the suffix to add to the trend surface column (or SpatRaster layer) names in the output data. The default is "_TS".
}
  \item{save.models}{
logical value indicating whether the models obtained from the regressions should be saved and included in the output. The default is FALSE.
}
  \item{verbosity}{
integer value indicating the amount of messages to display; currently meaningful values are 0, 1, and 2 (the default).
}
  \item{na.rm}{
logical value (default TRUE), used if 'data' is a SpatRaster, indicating whether NA input pixels should be NA in the output.
}
  \item{\dots}{
  additional arguments to be passed to \code{\link{modelTrim}} (if step = TRUE and criterion = "significance").
  }
}
\details{
Trend Surface Analysis is a way to model the spatial structure in species' distributions by regressing occurrence data on the spatial coordinates x and y, for a linear trend, or on polynomial terms of these coordinates (x^2, y^2, x*y, etc.), for curvilinear trends (Legendre & Legendre, 1998; Borcard et al., 2011). Second- and third-degree polynomials are often used. 'multTSA' allows specifying the degree of the spatial polynomial to use. By default, it uses a 3rd-degree polynomial and performs stepwise AIC selection of the polynomial terms to include.
}
\value{
If 'data' inherits class 'data.frame', the function returns a data frame containing the identifier column (if provided in 'id.col') and one column per species containing the value predicted by the trend surface analysis. If 'data' is a SpatRaster, the output is a SpatRaster of the values predicted by the trend surface analysis. If save.models = TRUE, the output is a list containing this and a list of the model objects.
}
\references{
Borcard D., Gillet F. & Legendre P. (2011) Numerical Ecology with R. Springer, New York.

Legendre P. & Legendre L. (1998) Numerical Ecology. Elsevier, Amsterdam.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{distPres}}, \code{\link{poly}}, \code{\link{multGLM}}
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
