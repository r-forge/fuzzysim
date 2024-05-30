\name{FDR}
\alias{FDR}
\title{
False Discovery Rate
}
\description{
Calculate the false discovery rate (type I error) under repeated testing and determine which variables to select and to exclude from multivariate analysis.
}
\usage{
FDR(data = NULL, sp.cols = NULL, var.cols = NULL, pvalues = NULL,
test = "Chisq", model.type = NULL, family = "auto", correction = "fdr",
q = 0.05, verbose = NULL, verbosity = 1, simplif = FALSE)
}
\arguments{
  \item{data}{a data frame containing the response and predictor variables
  (one in each column).}
  \item{sp.cols}{name or index number of the column containing the response variable
  (currently implemented for only one response variable at a time).}
  \item{var.cols}{names or index numbers of the columns containing the predictor variables.}
  \item{pvalues}{optionally, instead of 'data', 'sp.cols' and 'var.cols', a data frame with the names of the predictor variables in the first column andtheir bivariate p-values (obtained elsewhere) in the second column. Example: pvalues <- data.frame(var = letters[1:5], pval = c(0.02, 0.004, 0.07, 0.03, 0.05)).}
  \item{test}{(if 'pvalues' not provided) argument to pass to \code{\link{anova}} to obtain the p-value for each variable. Should be one of "Chisq" (currently the default, for back-compatibility), "Rao", "LRT" or "F" (the latter is not appropriate for models of family "binomial").}
  \item{model.type}{this argument (previously a character value, either "LM" or "GLM") is now deprecated and ignored with a warning if provided. This information is now included in argument 'family' -- e.g., if you want linear models (LM), you can set 'family = "gaussian"'.}
  \item{family}{The error distribution and (optionally) the link function to use (see \code{\link{glm}} or \code{\link{family}} for details). The default "auto" automatically uses "binomial" family for response variables containing only values of 0 and 1; "poisson" for positive integer responses (i.e. count data); "Gamma" for positive non-integer; and "gaussian" (i.e., linear models) otherwise.}
  \item{correction}{the correction procedure to apply to the p-values; see
  \code{\link{p.adjust.methods}} for available options and \code{\link{p.adjust}}
  for more information. The default is "fdr".}
  \item{q}{the threshold value of FDR-corrected significance above which to
  reject variables. Defaults to 0.05.}
  \item{verbose}{deprecated argument, replaced by 'verbosity' (below).}
  \item{verbosity}{integer value indicating the amount of messages to display. The default is 1, for a medium amount of messages. Use 2 for more messages.}
  \item{simplif}{logical value indicating if simplified results should be provided (see Value).}
}
\details{
It is common in ecology to search for statistical relationships between species' occurrence and a set of predictor variables. However, when a large number of variables is analysed (compared to the number of observations), false findings may arise due to repeated testing. Garcia (2003) recommended controlling the false discovery rate (FDR; Benjamini & Hochberg 1995) in ecological studies. The \code{\link{p.adjust}} R function performs this and other corrections to the significance (p) values of variables under repeated testing. The 'FDR' function performs repeated regressions (either linear or logistic) or uses already-obtained p values for a set of variables; calculates the FDR with 'p.adjust'; and shows which variables should be retained for or excluded from further multivariate analysis according to their corrected p values (see, for example, Barbosa, Real & Vargas 2009).

The FDR function uses the Benjamini & Hochberg ("BH", alias "fdr") correction by default, but check the \code{\link{p.adjust}} documentation for other available methods, namely "BY", which allows for non-independent data. Input data may be the response variable (for example, the presence-absence or abundance of a species) and the predictors (a table with one predictor variable in each column, with the same number of rows and in the same order as the response). Alternatively, you may already have performed the univariate regressions and have a set of variables and corresponding p values which you want to correct with FDR; in this case, get a table with your variables' names in the first column and their p values in the second column, and supply it as the 'pvalues' argument (no need to provide response or predictors in this case).
}
\value{
If simplif = TRUE, this function returns a data frame with the variables' names as row names and 4 columns containing, respectively, their individual (bivariate) coefficients against the response, their individual AIC (Akaike's Information Criterion; Akaike, 1973), BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC, SBIC; Schwarz, 1978), p-value and adjusted p-value according to the applied 'correction'.
If simplif = FALSE (the default), the result is a list of two such data frames:
\item{exclude}{with the variables to exclude.}
\item{select}{with the variables to select (under the given 'q' value).}
}
\references{
Akaike, H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Barbosa A.M., Real R. & Vargas J.M (2009) Transferability of environmental favourability models in geographic space: The case of the Iberian desman (Galemys pyrenaicus) in Portugal and Spain. Ecological Modelling 220: 747-754

Benjamini Y. & Hochberg Y. (1995) Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society, Series B 57: 289-300

Garcia L.V. (2003) Controlling the false discovery rate in ecological research. Trends in Ecology and Evolution 18: 553-554

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{p.adjust}}
}
\examples{
data(rotif.env)

names(rotif.env)

FDR(data = rotif.env, sp.cols = 18, var.cols = 5:17)

FDR(data = rotif.env, sp.cols = 18, var.cols = 5:17, simplif = TRUE)

my_pvalues <- data.frame(var = letters[1:5], pval = c(0.02, 0.004, 0.07, 0.03, 0.05))
FDR(pvalues = my_pvalues)
}
\keyword{ multivariate }
