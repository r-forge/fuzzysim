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
argument to pass to \code{\link{drop1}} specifying the statistical test whose 'p.out' a variable must exceed to be expelled from the model (if it does not simultaneously pass the 'test.in' when direction="both"). Can be "LRT" (the default), "Rao", "Chisq" or "F".
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
logical, whether to print the Wald test statistics using \code{\link{summaryWald}}, rather than the z test normally returned by \code{\link{summary}}. Used only if trace > 1. Requires the \pkg{aod} package. The default is FALSE.
}
}
\details{
Stepwise variable selection is a way of selecting a subset of significant variables to get a simple and easily interpretable model. It is more computationally efficient than best subset selection. This function uses the R functions \code{\link{add1}} for selecting and \code{\link{drop1}} for excluding variables. The default parameters mimic the "Forward Selection (Conditional)" stepwise procedure implemented in the IBM SPSS software. This is a widely used (e.g. Munoz et al. 2005, Olivero et al. 2017, 2020, Garcia-Carrasco et al. 2021) but also widely criticized (e.g. Harrell 2001; Whittingham et al. 2006; Flom & Cassell, 2007; Smith 2018) method for variable selection, though its AIC-based counterpart (implemented in the \code{\link{step}} R function) is equally flawed (e.g. Murtaugh 2014; Coelho et al. 2019).
}
\value{
If simplif=TRUE (the default), this function returns the model object obtained after the variable selection procedure. If simplif=FALSE, it returns a list with the following components:
\item{model}{the model object obtained after the variable selection procedure.}
\item{steps}{a data frame where each row shows the variable included or excluded at each step.}
\item{predictions}{(if preds=TRUE) a data frame where each column contains the predictions of the model obtained at each step. These predictions are probabilities by default, or favourabilities if Favourability=TRUE.}
}
\references{
Coelho M.T.P., Diniz-Filho J.A. & Rangel T.F. (2019) A parsimonious view of the parsimony principle in ecology and evolution. Ecography, 42:968-976

Flom P.L. & Cassell D.L. (2007) Stopping stepwise: Why stepwise and similar selection methods are bad, and what you should use. NESUG 2007

Garcia-Carrasco J.M., Munoz A.R., Olivero J., Segura M. & Real R. (2021) Predicting the spatio-temporal spread of West Nile virus in Europe. PLoS Neglected Tropical Diseases 15(1):e0009022

Harrell F.E. (2001) Regression modeling strategies: With applications to linear models, logistic regression, and survival analysis. Springer-Verlag, New York

Munoz, A.R., Real R., Barbosa A.M. & Vargas J.M. (2005) Modelling the distribution of Bonelli's Eagle in Spain: Implications for conservation planning. Diversity and Distributions 11: 477-486

Murtaugh P.A. (2014) In defense of P values. Ecology, 95:611-617

Olivero J., Fa J.E., Real R., Marquez A.L., Farfan M.A., Vargas J.M, Gaveau D., Salim M.A., Park D., Suter J., King S., Leendertz S.A., Sheil D. & Nasi R. (2017) Recent loss of closed forests is associated with Ebola virus disease outbreaks. Scientific Reports 7: 14291

Olivero J., Fa J.E., Farfan M.A., Marquez A.L., Real R., Juste F.J., Leendertz S.A. & Nasi R. (2020) Human activities link fruit bat presence to Ebola virus disease outbreaks. Mammal Review 50:1-10

Smith G. (2018) Step away from stepwise. Journal of Big Data 32 (https://doi.org/10.1186/s40537-018-0143-6)

Whittingham M.J., Stephens P.A., Bradbury R.B. & Freckleton R.P. (2006) Why do we still use stepwise modelling in ecology and behaviour? Journal of Animal Ecology, 75:1182-1189
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{step}}, \code{\link{stepByStep}}, \code{\link{modelTrim}}
}
\examples{
data(rotif.env)

stepwise(data = rotif.env, sp.col = 21, var.cols = 5:17)

sw <- stepwise(data = rotif.env, sp.col = 21, var.cols = 5:17, simplif = FALSE)
sw
}
