\name{corSelect}
\alias{corSelect}
\title{
Select among correlated variables based on a given criterion
}
\description{
This function computes pairwise correlations among the variables in a dataset and, among each pair of variables correlated above a given threshold (or, optionally, below a given significance value), it excludes the variable with either the highest variance inflation factor (VIF), or the weakest, least significant or least informative bivariate (individual) relationship with the response variable, according to a specified criterion.
}
\usage{
corSelect(data, sp.cols = NULL, var.cols, coeff = TRUE,
cor.thresh = ifelse(isTRUE(coeff), 0.8, 0.05),
select = ifelse(is.null(sp.cols), "VIF", "cor"), test = "Chisq",
family = "auto", use = "pairwise.complete.obs", method = "pearson",
verbosity = 1)
}
\arguments{
  \item{data}{a data frame containing the response and predictor variables.}
  \item{sp.cols}{optional, name or index number of the column of 'data' that contains the response (e.g. species) variable. Currently, only one 'sp.cols' can be used at a time, so an error message is returned if length(sp.cols) > 1. If left NULL, 'select' will be "VIF" by default.}
  \item{var.cols}{names or index numbers of the columns of 'data' that contain the predictor variables.}
  \item{coeff}{logical value  (default TRUE) indicating whether two variables should be considered highly correlated based on the magnitude of their coefficient of correlation. If set to FALSE, this classification will be based instead on the p-value of the correlation, but mind that (with sufficient sample size) correlations can be statistically significant even if weak.}
  \item{cor.thresh}{if coeff=TRUE (the default): threshold value of absolute correlation coefficient (default 0.8) above which two predictor variables are considered highly correlated. If coeff=FALSE: threshold value of p-value (default 0.05) below which two predictor variables are considered highly (or significantly) correlated.}
  \item{select}{character value indicating the criterion for excluding a variable within each highly correlated pair. Can be "VIF" (the default if 'sp.cols' is NULL), "cor" (the default if 'sp.cols' is specified), "p.value"," "AIC" or "BIC" (see Details).}
  \item{test}{argument to pass to the \code{\link{FDR}} function (which, in turn, passes it to \code{\link{anova}}) if test="p.value". The default is "Chisq" for back-compatibility.}
\item{family}{If 'select' is "p.value", "AIC" or "BIC", the error distribution and (optionally) the link function to use for assessing significant / informative variables (see \code{\link{glm}} or \code{\link{family}} for details). The default "auto" automatically uses "binomial" family for response variables containing only values of 0 and 1; "poisson" for positive integer responses (i.e. count data); "Gamma" for positive non-integer; and "gaussian" (i.e., linear models) otherwise.}
  \item{use}{argument to pass to \code{\link{cor}} indicating which rows to use when there are missing values. Can be "pairwise.complete.obs" (the default here), "everything", "all.obs", "complete.obs", "na.or.complete".}
  \item{method}{argument to pass to \code{\link{cor}} specifying the correlation coefficient to use. Can be "pearson" (the default, with a recommended minimum of 30 rows of data), "kendall", or "spearman" (with a recommended minimum of 10 rows of data).}
  \item{verbosity}{integer value indicating the amount of messages to display. The default is 1, for a medium amount of messages. Use 2 for more messages.}
}
\details{
Correlations among variables are often considered problematic in multivariate models, as they inflate the variance of coefficients and thus may bias the interpretation of the effects of those variables on the response (Legendre & Legendre 2012). Note, however, that the perceived problem often stems from misconceptions about the interpretation of multiple regression models, and that removing (albeit correlated) variables usually reduces predictive power (Morrissey & Ruxton 2018, Gregorich et al. 2021, Vanhove 2021). Removing high correlations is, however, a way of reducing the number of variables to include in a model, when the potentially meaningful variables are still numerous and no better a priori selection criterion is available.

One of the strategies to reduce correlations within a dataset consists of excluding one from each pair of highly correlated variables. However, it is not always straightforward (or ecological knowledge is not alway sufficient) to choose which variable to exclude. This function selects among pairwise-correlated variables based either on their variance inflation factor (VIF: Marquardt 1970; Mansfield & Helms 1982) within the variables dataset (obtained with the \code{\link{multicol}} function and recalculated iteratively after each variable exclusion); or on their relationship with the response, either by simply computing the \code{\link{cor}}relation between each variable and the response and excluding the variable with the smallest absolute coefficient; or by building a bivariate generalized linear model (\code{\link{glm}}) of each variable against the response and excluding, among each of two correlated variables, the one with the largest (worst) p-value, AIC (Akaike's Information Criterion: Akaike, 1973) or BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC or SBIC: Schwarz, 1978), which is calculated with the \code{\link{FDR}} function.

If 'select' is NULL, or if 'select' is other than "VIF" but 'sp.cols' is NULL, the function returns only a table showing the pairs of variables that are correlated beyond the given threshold, without selection or exclusion. If the 'select' criterion requires assessing bivariate relationships and 'sp.cols' is provided, the function uses only the rows of the dataset where 'sp.cols' (used as the response variable) contains finite values against which the predictor variables can be modelled; rows with NA or NaN in 'sp.cols' are thus excluded from the calculation of correlations among predictor variables.
}
\value{
This function returns a list of 7 elements (unless select=NULL, in which case it returns only the first of these elements):
\item{high.correlations}{data frame showing the pairs of input variables that are correlated beyond the given threshold, their correlation coefficient and its associated p-value.}
\item{bivariate.significance}{data frame with the individual p-value, AIC, BIC and correlation coefficient (if one of these was the 'select' criterion and if 'sp.cols' was provided) of each of the highly correlated variables against the response variable.}
\item{excluded.vars}{character vector containing the names of the variables to exclude (i.e., from each highly correlated pair, the variable with the worse 'select' score.}
\item{selected.vars}{character vector containing the names of the variables to select (i.e., the non-correlated variables and, from each correlated pair, the variable with the better 'select' score).}
\item{selected.var.cols}{integer vector containing the column indices of the selected variables in 'data'.}
\item{strongest.remaining.corr}{numerical value indicating the strongest correlation coefficient among the selected variables.}
\item{remaining.multicollinearity}{data frame showing the \code{\link{multicol}}linearity among the selected variables.}
}
\references{
Akaike H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Gregorich M., Strohmaier S., Dunkler D. & Heinze G. (2021) Regression with Highly Correlated Predictors: Variable Omission Is Not the Solution. Int. J. Environ. Res. Public Health, 18: 4259.

Legendre P. & Legendre L. (2012) Numerical ecology (3rd edition). Elsevier, Amsterdam: 990 pp.

Marquardt D.W. (1970) Generalized inverses, ridge regression, biased linear estimation, and nonlinear estimation. Technometrics 12: 591-612.

Mansfield E.R. & Helms B.P. (1982) Detecting multicollinearity. The American Statistician 36: 158-160.

Morrissey M.B. & Ruxton G.D. (2018) Multiple Regression Is Not Multiple Regressions: The Meaning of Multiple Regression and the Non-Problem of Collinearity. Philosophy, Theory, and Practice in Biology, 10: 003. DOI: 10.3998/ptpbio.16039257.0010.003

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.

Vanhove J. (2021)  Collinearity isn't a disease that needs curing. Meta-Phsychology 5, MP.2020.2548. DOI: 10.15626/MP.2021.2548
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{multicol}}, \code{\link{FDR}}, \code{\link{cor}}; and \code{collinear} in package \pkg{collinear}, which handles also categorical variables and allows specifying a preference order for the variables to keep.
}
\examples{
data(rotif.env)

corSelect(rotif.env, var.cols = 5:17, select = NULL)

corSelect(rotif.env, var.cols = 5:17)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, cor.thresh = 0.7)

corSelect(rotif.env, sp.cols = 46, var.cols = 5:17, select = "BIC", method = "spearman")
}
\keyword{ multivariate }
