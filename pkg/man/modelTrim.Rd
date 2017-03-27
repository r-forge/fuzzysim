\name{modelTrim}
\alias{modelTrim}
\title{
Trim off non-significant variables from a model
}
\description{
This function performs a stepwise removal of non-significant variables from a model.
}
\usage{
modelTrim(model, method = "summary", alpha = 0.05)
}
\arguments{
  \item{model}{a model object.}
  \item{method}{the method for getting the individual p-values. Can be either "summary" for the p-values of the coefficient estimates, or "anova" for the p-values of the variables themselves (see Details).}
  \item{alpha}{the p-value above which a variable is removed.}
}
\details{
Stepwise variable selection is a common procedure for simplifying models. It maximizes predictive efficiency in an objective and reproducible way, and is useful when the individual importance of the predictors is not known a priori (Hosmer & Lemeshow, 2000). The \code{\link{step}} R function performs such procedure using an information criterion (AIC) to select the variables, but it often leaves variables that are not significant in the model. Such variables can be subsequently removed with a manual stepwise procedure (e.g. Crawley 2007, p. 442; Barbosa & Real 2010, 2012; Estrada & Arroyo 2012). The \code{modelTrim} function performs such removal automatically until all remaining variables are significant. It can also be applied to a full model (i.e., without previous use of the step function), as it serves as a backward stepwise selection procedure based on the significance of the coefficients (if \code{method = "summary"}, the default) or on the significance of the variables themselves (if \code{method = "anova"}, better when there are categorical variables in the model).
}
\value{
The input model object after removal of non-significant variables.
}
\references{
Barbosa A.M. & Real R. (2010) Favourable areas for expansion and reintroduction of Iberian lynx accounting for distribution trends and genetic diversity of the European rabbit. Wildlife Biology in Practice 6: 34-47

Barbosa A.M. & Real R. (2012) Applying fuzzy logic to comparative distribution modelling: a case study with two sympatric amphibians. The Scientific World Journal, Article ID 428206

Crawley M.J. (2007) The R Book. John Wiley & Sons, Chichester (UK)

Estrada A. & Arroyo B. (2012) Occurrence vs abundance models: Differences between species with varying aggregation patterns. Biological Conservation, 152: 37-45

Hosmer D. W. & Lemeshow S. (2000) Applied Logistic Regression (2nd ed). John Wiley and Sons, New York
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{step}}
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

summary(mod)  # contains non-significant variables


# use modelTrim to get rid of non-significan effects:

mod <- modelTrim(mod)

summary(mod)  # only significant variables now

}
\keyword{ models }

