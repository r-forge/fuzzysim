\name{stepByStep}
\alias{stepByStep}
\title{
Compare model predictions along a stepwise variable selection process
}
\description{
This function builds (or takes) a generalized linear model with stepwise inclusion of variables, using either AIC, BIC or p.value as the selection criterion; and it returns the values predicted at each step (i.e., as each variable is added or dropped), as well as their correlation with the final model predictions.
}
\usage{
stepByStep(data, sp.col, var.cols, family = binomial(link = "logit"),
Favourability = FALSE, trace = 0, direction = "both", select = "AIC",
k = 2, test.in = "Rao", test.out = "LRT", p.in = 0.05, p.out = 0.1,
cor.method = "pearson")
}
\arguments{
  \item{data}{
a data frame (or another object that can be coerced with "as.data.frame", e.g. a matrix, a tibble, a SpatVector) containing the response and predictor variables to model. Alternatively, a model object of class 'glm', from which the names, values and order of the variables will be taken -- arguments 'sp.col', 'var.cols', 'family', 'trace', 'direction', 'select', 'k', 'test.in', 'test.out', 'p.in' and 'p.out' will then be ignored.
}
  \item{sp.col}{
(if 'data' is not a model object) the name or index number of the column of 'data' that contains the response variable.
}
  \item{var.cols}{
(if 'data' is not a model object) the names or index numbers of the columns of 'data' that contain the predictor variables.
}
  \item{family}{
(if 'data' is not a model object) argument to pass to \code{\link{glm}} indicating the family (and error distribution) to use in modelling. The default is binomial distribution with logit link (for binary response variables).
}
  \item{Favourability}{
logical, whether to apply the \code{\link{Fav}}ourability function to remove the effect of prevalence from predicted probability (Real et al. 2006). Applicable only to binomial GLMs. Defaults to FALSE.
}
  \item{trace}{
(if 'data' is not a model object) argument to pass to \code{\link{step}} (if select="AIC" or "BIC") or to \code{\link{stepwise}} (if select="p.value"). If positive, information is printed during the stepwise procedure. Larger values may give more detailed information. The default is 0 (silent).
}
  \item{direction}{
(if 'data' is not a model object) argument to pass to \code{\link{step}} (if select="AIC" or "BIC") or to \code{\link{stepwise}} (if select="p.value"). Can be "forward" or "both". The default is the latter, to match related functions like \code{\link{step}}, \code{\link{stepwise}} and \code{\link{multGLM}}. (Note that older versions of this function had "forward" as the default.)
}
  \item{select}{(if 'data' is not a model object) character string specifying the criterion for stepwise selection of variables if step=TRUE. Options are the default "AIC" (Akaike's Information Criterion; Akaike, 1973); BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC or SBIC; Schwarz, 1978); or "p.value" (Murtaugh, 2014). The first two options imply using \code{\link{step}} as the variable selection function, while the last option calls the \code{\link{stepwise}} function.
}
  \item{k}{
(if 'data' is not a model object and select="AIC") argument passed to the \code{\link{step}} function indicating the multiple of the number of degrees of freedom used for the penalty. The default is 2, which yields the original AIC. You can use larger values for a more stringent selection-- e.g., for a critical p-value of 0.05, use k = qchisq(0.05, 1, lower.tail = F). If select="BIC", k is accordingly changed to log(n), being 'n' the number of complete rows of the response + variables dataframe (after removing missing values).
}
  \item{test.in}{
(if 'data' is not a model object and select="p.value") argument passed to \code{\link{add1}} specifying the statistical test whose 'p.in' a variable must pass to enter the model. Can be "Rao" (the default), "LRT", "Chisq" or "F".
}
  \item{test.out}{
(if 'data' is not a model object and select="p.value") argument passed to \code{\link{drop1}} specifying the statistical test whose 'p.out' a variable must exceed to be expelled from the model (if it does not simultaneously pass the 'test.in' when direction="both"). Can be "LRT" (the default), "Rao", "Chisq" or "F".
}
  \item{p.in}{
(if 'data' is not a model object and select="p.value") threshold p-value for a variable to enter the model. Defaults to 0.05.
}
  \item{p.out}{
(if 'data' is not a model object and select="p.value") threshold p-value for a variable to leave the model. Defaults to 0.1.
}
  \item{cor.method}{
character string to pass to \code{\link{cor}} indicating which coefficient to use for correlating predictions at each step with those of the final model. Can be "pearson" (the default), "kendall", or "spearman".
}
}
\details{
Stepwise variable selection often includes more variables than would a model selected after examining all possible combinations of the variables (e.g. with package \pkg{MuMIn} or \pkg{glmulti}). The 'stepByStep' function can be useful to assess if a stepwise model with just the first few variables could already provide predictions very close to the final ones (see e.g. Fig. 3 in Munoz et al., 2005). It can also be useful to see which variables determine the more general trends in the model predictions, and which variables just provide additional (local) nuances.
}
\value{
This function returns a list of the following components:
\item{predictions}{a data frame with the model's fitted values at each step of the variable selection.}
\item{correlations}{a numeric vector of the correlation between the predictions at each step and those of the final model.}
\item{variables}{a character vector of the variables in the final model, named with the step at which each was included.}
\item{model}{the resulting model object.}
}
\references{
Akaike, H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Munoz, A.R., Real R., Barbosa A.M. & Vargas J.M. (2005) Modelling the distribution of Bonelli's Eagle in Spain: Implications for conservation planning. Diversity and Distributions 11: 477-486

Murtaugh P.A. (2014) In defense of P values. Ecology, 95:611-617

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.
}
\author{
A. Marcia Barbosa, with contribution by Alba Estrada
}
\seealso{
\code{\link{step}}, \code{\link{glm}}, \code{\link{modelTrim}}
}
\examples{
data(rotif.env)

stepByStep(data = rotif.env, sp.col = 21, var.cols = 5:17)

stepByStep(data = rotif.env, sp.col = 21, var.cols = 5:17, select = "p.value")


# with a model object:

form <- reformulate(names(rotif.env)[5:17], names(rotif.env)[21])
mod <- step(glm(form, data = rotif.env))

stepByStep(data = mod)
}
