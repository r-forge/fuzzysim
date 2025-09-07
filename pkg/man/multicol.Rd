\name{multicol}
\alias{multicol}
\title{Analyse multicollinearity in a dataset, including VIF}
\description{
This function analyses multicollinearity in a set of variables or in a model, including the R-squared, tolerance and variance inflation factor (VIF).
}
\usage{
multicol(vars = NULL, model = NULL, reorder = TRUE)
}
\arguments{
  \item{vars}{
A matrix, an object inheriting class data.frame, or a multi-layer SpatRaster containing the numeric variables for which to calculate multicollinearity. Note that only the 'independent' (predictor, explanatory, right hand side) variables should be entered, as the result obtained for each variable depends on all the other variables present in the analysed data set.
}
  \item{model}{
Alternatively to 'vars', a model object of class "glm" to calculate 'multicol' among the included variables.
}
  \item{reorder}{
logical, whether variables should be output in decreasing order or VIF value rather than in their input order. The default is TRUE.
}
}
\details{
Testing collinearity among covariates is a recommended step of data exploration before applying a statistical model (Zuur et al. 2010). However, you can also calculate multicollinearity among the variables already included in a model.

The multicol function calculates the degree of multicollinearity in a set of numeric variables, using three closely related measures: R squared (the coefficient of determination of a linear regression of each predictor variable on all other predictor variables, i.e., the amount of variation in each variable that is accounted for by other variables in the dataset); tolerance (1 - R squared), i.e. the amount of variation in each variable that is not included in the remaining variables; and the variance inflation factor: VIF = 1 / (1 - R squared), which, in a linear model with these variables as predictors, reflects the degree to which the variance of an estimated regression coefficient is increased due only to the correlations among covariates (Marquardt 1970; Mansfield & Helms 1982).
}
\value{
The function returns a matrix with one row per analysed variable, the names of
the variables as row names, and 3 columns: R-squared, Tolerance and VIF.
}
\references{
Marquardt D.W. (1970) Generalized inverses, ridge regression, biased linear estimation, and nonlinear estimation. Technometrics 12: 591-612.

Mansfield E.R. & Helms B.P. (1982) Detecting multicollinearity. The American Statistician 36: 158-160.

Zuur A.F., Ieno E.N. & Elphick C.S. (2010) A protocol for data exploration to avoid common statistical problems. Methods in Ecology and Evolution 1: 3-14.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{vif} in package \pkg{HH}, \code{vif} in package \pkg{usdm}
}
\examples{
data(rotif.env)
names(rotif.env)

# calculate multicollinearity among the predictor variables:
multicol(rotif.env[ , 5:17], reorder = FALSE)
multicol(rotif.env[ , 5:17])

# you can also calculate multicol among the variables included in a model:
mod <- step(glm(Abrigh ~ Area + Altitude + AltitudeRange +
HabitatDiversity + HumanPopulation + Latitude + Longitude +
Precipitation + PrecipitationSeasonality + TemperatureAnnualRange
+ Temperature + TemperatureSeasonality + UrbanArea,
data = rotif.env))
multicol(model = mod)

# more examples using R datasets:
multicol(trees)

# you'll get a warning and some NA results if any of the variables
# is not numeric:
multicol(OrchardSprays)

# so define the subset of numeric 'vars' to calculate 'multicol' for:
multicol(OrchardSprays[ , 1:3])
}
\keyword{multivariate}
