\name{multicol}
\alias{multicol}
\title{Analyse the multicollinearity in a dataset, including VIF}
\description{
This function analyses multicollinearity in a set of variables, including the R-squared, tolerance and variance inflation factor (VIF).
}
\usage{
multicol(vars)
}
\arguments{
  \item{vars}{
A matrix or data frame containing the numeric variables for which to calculate
multicollinearity. Only the 'independent' (predictor, explanatory, right hand side)
variables should be entered, as the result obtained for each variable depends on all
the other variables present in the analysed data set.
}
}
\details{
The multicol function calculates the degree of multicollinearity in a set of
numeric variables, using three closely related measures: R squared (the coefficient
of determination of a linear regression of each predictor variable on all other
predictor variables, i.e., the amount of variation in each variable that is accounted
for by other variables in the dataset); tolerance (1 - R squared), i.e. the amount of
variation in each variable that is not included in the remaining variables; and the
variance inflation factor: VIF = 1 / (1 - R squared), which reflects the degree to
which the variance of an estimated regression coefficient (in a model using these
variables as predictors) is increased due only to the correlations among covariates.
(Marquardt 1970; Mansfield & Helms 1982). Such measures, especially the VIF, are
widely used in ecological modelling and a range of other research areas, and several
packages already calculate the VIF in R, but their results can be strikingly different
(Estrada & Barbosa, submitted). The \code{multicol} function calculates
multicollinearity statistics directly on the independent variables, producing the
expected results and without (so far) clashing with other packages.
}
\value{
The function returns a matrix with one row per analysed variable, the names of
the variables as row names, and 3 columns: R-squared, Tolerance and VIF.
}
\references{
Estrada A. & Barbosa A.M. (submitted) Calculating the Variance Inflation Factor
in R: cautions and recommendations.

Marquardt D.W. (1970) Generalized inverses, ridge regression, biased linear
estimation, and nonlinear estimation. Technometrics 12: 591-612.

Mansfield E.R. & Helms B.P. (1982) Detecting multicollinearity. The American
Statistician 36: 158-160.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
}
\examples{
data(rotif.env)
names(rotif.env)

# calculate multicollinearity among the predictor variables:
multicol(rotif.env[ , 5:17])


# more examples uing R datasets:
multicol(trees)

# you'll get a warning and some NA results if any of the variables is not numeric:
multicol(OrchardSprays)

# so define the subset of numeric 'vars' to calculate 'multicol' for:
multicol(OrchardSprays[ , 1:3])
}
\keyword{multivariate}
