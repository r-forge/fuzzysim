\name{corSelect}
\alias{corSelect}
\title{
Select among correlated variables based on their relationship with the response
}
\description{
This function calculates pairwise correlations among the variables in a dataset and, among each pair of variables correlated above a given threshold, selects the one with the most significant or most informative bivariate (individual) relationship with the response variable.
}
\usage{
corSelect(data, sp.cols, var.cols, cor.thresh = 0.8, select = "p.value", ...)
}
\arguments{
  \item{data}{a data frame containing the response and predictor variables.}
  \item{sp.cols}{index number of the column of \code{data} that contains the response (e.g. species) variable.}
  \item{var.cols}{index numbers of the columns of \code{data} that contain the predictor variables.}
  \item{cor.thresh}{threshold value of correlation coefficient above which predictor variables should be excluded. The default is 0.8.}
  \item{select}{character value indicating the criterion for selecting variables (among those that are correlated) based on their relationship with the response variable. Can be "p.value" (the default) or "AIC". A message is displayed saying whether the result would have been different with the alternative criterion.}
  \item{\dots}{additional arguments to pass to \code{\link{cor}}, namely the \code{method} to use (either \code{"pearson"}, \code{"kendall"} or \code{"spearman"} correlation coefficient; the first is the default).}
}
\details{
Correlations among variables are problematic in multivariate models, as they inflate the variance of coefficients and thus may bias the interpretation of the effects of those variables on the response (Legendre & Legendre 2012). One of the strategies to circumvent this problem is to eliminate a priori one from each pair of correlated variables, but it is not always straighforward to choose the right variable. This function selects such variables based on their relationship with the response variable, by building a bivariate model of each individual variable against the response and choosing, among each of two correlated variables, the one with the smaller (best) AIC or p-value.

The function uses only the rows of the dataset where the response variable contains finite values against which the predictor variables can be modelled. Rows with NA or NaN in the response variable are excluded from the calculation of correlations among predictor variables).
}
\value{
This function returns a list of 7 elements:
\item{high.correlations}{data frame showing the pairs of input variables that are correlated above the threshold.}
\item{bivariate.significance}{data frame with the individual AIC (Akaike's Information Criterion) and significance (p-value) of each of the highly correlated variables against the response variable.}
\item{excluded.vars}{character vector containing the names of the variables to be excluded (i.e., from each highly correlated pair, the variable with larger AIC or p-value against the response).}
\item{selected.vars}{character vector containing the names of the variables to be selected (i.e., the non-correlated variables and, from each correlated pair, the variable with smaller AIC or p-value against the response).}
\item{selected.var.cols}{integer vector containing the column indices of the variables to be selected in \code{data}.}
\item{strongest.remaining.corr}{numerical value indicating the highest (absolute) correlation coefficient among the selected variables.}
\item{remaining.multicollinearity}{data frame showing the \code{\link{multicol}}linearity among the selected variables.}
}
\references{
Legendre P. & Legendre L. (2012) Numerical ecology (3rd edition). Elsevier, Amsterdam: 990 pp.
}
\author{A. Marcia Barbosa}
\note{
}
\seealso{
\code{\link{multicol}}, \code{\link{FDR}}, \code{\link{cor}}
}
\examples{
data(rotif.env)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, cor.thresh = 0.7)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, method = "spearman")
}
\keyword{ multivariate }
