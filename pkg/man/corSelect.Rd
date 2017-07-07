\name{corSelect}
\alias{corSelect}
\title{
Select among correlated variables based on a given criterion
}
\description{
This function calculates pairwise correlations among the variables in a dataset and, among each pair of variables correlated above a given threshold, excludes the variable with either the highest variance inflation factor (VIF), or the least significant or least informative bivariate (individual) relationship with the response variable (if supplied), according to a specified criterion.
}
\usage{
corSelect(data, sp.cols = NULL, var.cols, cor.thresh = 0.8, 
select = "p.value", ...)
}
\arguments{
  \item{data}{a data frame containing the response and predictor variables.}
  \item{sp.cols}{index number of the column of 'data' that contains the response (e.g. species) variable. Currently, only one 'sp.cols' can be used at the same time, so an error message is returned if \code{length(sp.cols) > 1}. If \code{sp.cols = NULL}, the function returns only the pairs of variables that are correlated over the given threshold.}
  \item{var.cols}{index numbers of the columns of 'data' that contain the predictor variables.}
  \item{cor.thresh}{threshold value of correlation coefficient above which predictor variables should be excluded. The default is 0.8.}
  \item{select}{character value indicating the criterion for excluding variables among those that are correlated. Can be "p.value" (the default), "AIC", "BIC", or "VIF" (see Details).}
  \item{\dots}{additional arguments to pass to \code{\link{cor}}, namely the \code{method} to use (either "pearson", "kendall" or "spearman" correlation coefficient; the first is the default) and the way to deal with missing values (use = "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs").}
}
\details{
Correlations among variables are problematic in multivariate models, as they inflate the variance of coefficients and thus may bias the interpretation of the effects of those variables on the response (Legendre & Legendre 2012). One of the strategies to circumvent this problem is to eliminate one from each pair of correlated variables, but it is not always straightforward to choose the right variable to exclude a priori.

This function selects among correlated variables, based either on their variance inflation factor (VIF: Marquardt 1970; Mansfield & Helms 1982) within the variables dataset (obtained with the \code{\link{multicol}} function and recalculated iteratively after each variable exclusion); or on their relationship with the response, by building a bivariate model of each individual variable against the response and excluding, among each of two correlated variables, the one with the largest (worst) p-value, AIC (Akaike's Information Criterion: Akaike, 1973) or BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC or SBIC: Schwarz, 1978), which it calculates with the \code{\link{FDR}} function.

If \code{sp.cols} is left NULL and the 'select' criterion is other than "VIF", the function returns only the pairs of variables that are correlated above the given threshold. If the 'select' criterion requires assessing bivariate relationships and \code{sp.cols} is provided, the function uses only the rows of the dataset where this column (used as the response variable) contains finite values against which the predictor variables can be modelled; rows with NA or NaN in \code{sp.cols} are thus excluded from the calculation of correlations among predictor variables.
}
\value{
This function returns a list of 7 elements, unless \code{sp.cols = NULL}, in which case it returns only the first of these elements:
\item{high.correlations}{data frame showing the pairs of input variables that are correlated above the given threshold, and their correlation coefficient.}
\item{bivariate.significance}{data frame with the individual p-value, AIC and BIC (if one of these was the 'select' criterion) of each of the highly correlated variables against the response variable.}
\item{excluded.vars}{character vector containing the names of the variables to be excluded (i.e., from each highly correlated pair, the variable with the larger (worse) 'select' score.}
\item{selected.vars}{character vector containing the names of the variables to be selected (i.e., the non-correlated variables and, from each correlated pair, the variable with the smaller (better) 'select' score).}
\item{selected.var.cols}{integer vector containing the column indices of the selected variables in 'data'.}
\item{strongest.remaining.corr}{numerical value indicating the highest (absolute) correlation coefficient among the selected variables.}
\item{remaining.multicollinearity}{data frame showing the \code{\link{multicol}}linearity among the selected variables.}
}
\references{
Akaike, H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Legendre P. & Legendre L. (2012) Numerical ecology (3rd edition). Elsevier, Amsterdam: 990 pp.

Marquardt D.W. (1970) Generalized inverses, ridge regression, biased linear estimation, and nonlinear estimation. Technometrics 12: 591-612.

Mansfield E.R. & Helms B.P. (1982) Detecting multicollinearity. The American Statistician 36: 158-160.

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{multicol}}, \code{\link{FDR}}, \code{\link{cor}}
}
\examples{
data(rotif.env)

corSelect(rotif.env, var.cols = 5:17)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, cor.thresh = 0.7)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, method = "spearman")
}
\keyword{ multivariate }
