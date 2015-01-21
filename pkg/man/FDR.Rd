\name{FDR}
\alias{FDR}
\title{
False Discovery Rate
}
\description{
Calculate the false discovery rate (type I error) under repeated testing and determine which variables to select and to exclude from multivariate analysis.
}
\usage{
FDR(data = NULL, sp.cols = NULL, var.cols = NULL, pvalues = NULL, model.type,
family = "binomial", correction = "fdr", q = 0.05, verbose = TRUE)
}
\arguments{
  \item{data}{a data frame containing the response and predictor variables
  (one in each column).}
  \item{sp.cols}{index number of the column containing the response variable
  (currently implemented for only one response variable at a time).}
  \item{var.cols}{index numbers of the columns containing the predictor variables.}
  \item{pvalues}{optionally, instead of \code{data}, \code{sp.cols} and \code{var.cols},
  a data frame with the names of the predictor variables in the first column and
  their bivariate p-values (obtained elsewhere) in the second column. Example:
  pvalues <- data.frame(var = letters[1:5], pval = c(0.02, 0.004, 0.07, 0.03, 0.05)).}
  \item{model.type}{either "LM" (linear model, for continuous response variables)
  or "GLM" (generalized linear models, for binary or other variables for which
  such models are more appropriate)}
  \item{family}{if \code{model.type = "GLM"}, the error distribution and link
  function (see \code{\link{glm}} or \code{\link{family}} for details); defaults
  to "binomial" (for binary logistic regression).}
  \item{correction}{the correction procedure to apply to the p-values; see
  \code{\link{p.adjust.methods}} for available options and \code{\link{p.adjust}}
  for more information. The default is "fdr".}
  \item{q}{the threshold value of FDR-corrected significance above which to
  reject variables. Defaults to 0.05.}
  \item{verbose}{logical, whether to report a short description of the results.}
}
\details{
It is common in ecology to search for statistical relationships between species'
occurrence and a set of predictor variables. However, when a large number of
variables is analysed (compared to the number of observations), false findings
may arise due to repeated testing. Garcia (2003) recommended controlling the
false discovery rate (FDR; Benjamini & Hochberg 1995) in ecological studies.
The \code{\link{p.adjust}} R function performs this and other corrections to
the significance (p) values of variables under repeated testing. The \code{FDR}
function performs repeated regressions (either linear or binary logistic) or
uses already-obtained p values for a set of variables; calculates the FDR with
\code{\link{p.adjust}}; and shows which variables should be retained for or
excluded from further multivariate analysis according to their corrected
p values (see, for example, Barbosa, Real & Vargas 2009).

The FDR function uses the Benjamini & Hochberg ("BH") correction by default,
but check the \code{\link{p.adjust}} documentation for other available methods.
Input data may be the response variable (for example, the presence-absence
or abundance of a species) and the predictors (a table with one independent
variable in each column, with the same number of rows and in the same order as
the response); there should be no missing values in the data. Model type can
be either "LM" (linear model) for continuous response variables such as
abundance, or "GLM" (generalized linear model) for binary responses such as
presence-absence. Alternatively, you may already have performed the univariate
regressions and have a set of variables and corresponding p values which you
want to correct with FDR; in this case, get a table with your variables' names
in the first column and their p values in the second column, and supply it as
the \code{pvalues} argument to the \code{FDR} function (no need to provide
response or predictors in this case).
}
\value{
A list with the following components:
\item{exclude}{a data frame of the variables to exclude under the chosen criteria,
including the variables' names, their bivariate coefficients against the response,
their p-value and adjusted p-value.}
\item{select}{a data frame similar to the above for the variables to select.}
}
\references{
Barbosa A.M., Real R. & Vargas J.M (2009) Transferability of environmental
favourability models in geographic space: The case of the Iberian desman
(Galemys pyrenaicus) in Portugal and Spain. Ecological Modelling 220: 747-754

Benjamini Y. & Hochberg Y. (1995) Controlling the false discovery rate: a
practical and powerful approach to multiple testing. Journal of the Royal
Statistical Society, Series B 57: 289-300

Garcia L.V. (2003) Controlling the false discovery rate in ecological research.
Trends in Ecology and Evolution 18: 553-554
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{p.adjust}}
}
\examples{
data(rotif.env)

names(rotif.env)

FDR(data = rotif.env, sp.cols = 18, var.cols = 5:17, model.type = "GLM")
}
\keyword{ multivariate }
