\name{stepwise}
\alias{stepwise}
\title{
Stepwise regression
}
\description{
This function runs a stepwise regression, selecting and/or excluding variables based on the significance (p-value) of the statistical tests implemented in the \code{\link{add1}} and \code{\link{drop1}} functions of R.
}
\usage{
stepwise(data, sp.col, var.cols, id.col = NULL, family = binomial(link="logit"), 
direction = "both", test.in = "Rao", test.out = "LRT", p.in = 0.05, p.out = 0.1, 
trace = 1, simplif = TRUE, preds = FALSE, Favourability = FALSE, Wald = FALSE)
}
\arguments{
  \item{data}{
a data frame (or an object that can be coerced with 'as.data.frame') containing your target and predictor variables.
}
  \item{sp.col}{
name or index number of the column of 'data' that contains the response variable.
}
  \item{var.cols}{
names or index numbers of the columns of 'data' that contain the predictor variables.
}
  \item{id.col}{
  (optional) name or index number of column containing the row identifiers (if defined, it will be included in the output 'predictions' data frame).
  }
  \item{family}{
argument to be passed to \code{\link{glm}} indicating the error distribution (and optionally the link function) to be used in the model. The default is binomial distribution with logit link (i.e. logistic regression, for binary response variables), and it is the only one that has been tested so far. If you try other options, please carefully check your results and let me know if you find a bug.
}
  \item{direction}{
the mode of stepwise search. Can be either "forward", "backward", or "both" (the default).
}
  \item{test.in}{
argument to pass to \code{\link{add1}} specifying the statistical test whose 'p.in' a variable must pass to enter the model. Can be "Rao" (the default), "LRT", "Chisq" or "F".
}
  \item{test.out}{
argument to pass to \code{\link{drop1}} specifying the statistical test whose 'p.out' a variable must fail to be expelled from the model. Can be "LRT" (the default), "Rao", "Chisq" or "F".
}
  \item{p.in}{
threshold p-value for a variable to enter the model. Defaults to 0.05.
}
  \item{p.out}{
threshold p-value for a variable to leave the model. Defaults to 0.1.
}
  \item{trace}{
if positive, information is printed to the console at each step. The default is 1, for naming each variable that was added or removed. With trace=2, the summary of the model at each step is also printed.
}
  \item{simplif}{
logical, whether to return a simpler output containing only the model object (the default), or a list with, additionally, a data frame with the variable included or excluded at each step.
}
  \item{preds}{
logical, whether to return also the predictions given by the model at each step. This argument is ignored if simplif=TRUE.
}
  \item{Favourability}{
logical, whether to convert the predictions (if preds=TRUE) with the \code{\link{Fav}} function. This argument is ignored if simplif=TRUE.
}
  \item{Wald}{
logical, whether to report the Wald test statistics (rather than the z test normally returned by \code{\link{summary}}). Used only if trace > 1. Requires the \pkg{aod} package.
}
}
\details{
Stepwise variable selection is a way of selecting a subset of significant variables to get a simple and easily interpretable model (e.g. Munoz et al., 2005). It is more computationally efficient than best subset selection. This function uses \code{\link{add1}} for selecting and \code{\link{drop1}} for excluding variables. The default parameters mimic the "Forward Selection (Conditional)" stepwise procedure implemented in the IBM SPSS software. This is a widely used, but also widely criticized method for variable selection (e.g. Harrell 2001; Flom & Cassell, 2007; Smith 2018), though its AIC-based counterpart (implemented in the \code{\link{step}} function) is also not without flaws (Real et al., in prep.).
}
\value{
If simplif=TRUE (the default), this function returns the model object obtained after the variable selection procedure. If simplif=FALSE, it returns a list with the following components:
\item{model}{the model object obtained after the variable selection procedure.}
\item{steps}{a data frame where each row shows the variable included or excluded at each step.}
\item{predictions}{(if preds=TRUE) a data frame where each column contains the predictions of the model at each step. These predictions are probabilities by default, or favourabilities if Favourability=TRUE.}
}
\references{
Flom P.L. & Cassell D.L. (2007) Stopping stepwise: Why stepwise and similar selection methods are bad, and what you should use. NESUG 2007

Harrell F.E. (2001) Regression modeling strategies: With applications to linear models, logistic regression, and survival analysis. Springer-Verlag, New York

Munoz, A.R., Real R., Barbosa A.M. & Vargas J.M. (2005) Modelling the distribution of Bonelli's Eagle in Spain: Implications for conservation planning. Diversity and Distributions 11: 477-486

Smith G. (2018) Step away from stepwise. Journal of Big Data 32 (https://doi.org/10.1186/s40537-018-0143-6)
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{step}}, \code{\link{stepByStep}}, \code{\link{modelTrim}}
}
\examples{
data(rotif.env)

stepwise(data = rotif.env, sp.col = 18, var.cols = 5:17)
}
