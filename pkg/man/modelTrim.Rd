\name{modelTrim}
\alias{modelTrim}
\title{
Trim off non-significant variables from a model
}
\description{
This function performs a stepwise removal of non-significant variables from a model object, following Crawley (2005, 2007).
}
\usage{
modelTrim(model, method = "summary", alpha = 0.05, verbosity = 2, phy = NULL)
}
\arguments{
  \item{model}{a model object of class 'lm', 'glm' or 'phylolm'.}
  \item{method}{the method for getting the p-value of each variable. Can be either "summary" for the p-values of the coefficient estimates, or (if the model class is 'lm' or 'glm') "anova" for the p-values of the variables themselves (see Details).}
  \item{alpha}{the threshold p-value above which a variable is to be removed.}
  \item{verbosity}{integer number indicating the amount of messages to display; the default is the maximum number of messages available.}
  \item{phy}{if 'model' is of class 'phylolm', the phylogenetic tree to pass to phylolm::phylolm() when re-computing the model after the removal of each non-significant variable.}
}
\details{
Stepwise variable selection is a common procedure for simplifying models. It maximizes predictive efficiency in an objective and reproducible way, and it is useful when the individual importance of the predictors is not known a priori (Hosmer & Lemeshow, 2000). The \code{\link{step}} R function performs such procedure using an information criterion (AIC) to select the variables, but it often leaves variables that are not significant in the model. Such variables can be subsequently removed with a stepwise procedure (e.g. Crawley 2005, p. 208; Crawley 2007, p. 442 and 601; Barbosa & Real 2010, 2012; Estrada & Arroyo 2012). The 'modelTrim' function performs such removal automatically until all remaining variables are significant. It can also be applied to a full model (i.e., without previous use of the 'step' function), as it serves as a backward stepwise selection procedure based on the significance of the coefficients (if method = "summary", the default) or on the significance of the variables (if method = "anova", better when there are categorical variables in the model). See also \code{\link{stepwise}} for a more complete stepwise selection procedure based on a data frame.
}
\value{
The updated input model object after stepwise removal of non-significant variables.
}
\references{
Barbosa A.M. & Real R. (2010) Favourable areas for expansion and reintroduction of Iberian lynx accounting for distribution trends and genetic diversity of the European rabbit. Wildlife Biology in Practice 6: 34-47

Barbosa A.M. & Real R. (2012) Applying fuzzy logic to comparative distribution modelling: a case study with two sympatric amphibians. The Scientific World Journal, Article ID 428206

Crawley, M.J. (2005) Statistics: An introdution using R. John Wiley & Sons, Ltd.

Crawley, M.J. (2007) The R Book. John Wiley & Sons, Ltd.

Estrada A. & Arroyo B. (2012) Occurrence vs abundance models: Differences between species with varying aggregation patterns. Biological Conservation, 152: 37-45

Hosmer D. W. & Lemeshow S. (2000) Applied Logistic Regression (2nd ed). John Wiley and Sons, New York
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{step}}, \code{\link{stepwise}}
}
\examples{
# load sample data:

data(rotif.env)

names(rotif.env)


# build a stepwise model of a species' occurrence based on
# some of the variables:

mod <- with(rotif.env, step(glm(Abrigh ~ Area + Altitude + AltitudeRange +
HabitatDiversity + HumanPopulation, family = binomial)))


# examine the model:

summary(mod)  # includes non-significant variables

# use modelTrim to get rid of those:

mod <- modelTrim(mod)

summary(mod)  # only significant variables remain
}
\keyword{ models }

