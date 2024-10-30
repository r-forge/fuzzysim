\name{prevalence}
\alias{prevalence}
\title{
Prevalence
}
\description{
Prevalence is the proportion of presences of a species in a dataset, which is required (together with presence probability) for computing \code{\link{Fav}}ourability.
}
\usage{
prevalence(obs, model = NULL, event = 1, na.rm = TRUE)
}
\arguments{
  \item{obs}{a vector or a factor of binary observations (e.g. 1 vs. 0, male vs. female, disease vs. no disease, etc.). This argument is ignored if 'model' is provided.}
  \item{model}{alternatively to 'obs', a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' will be extracted with 'modEvA::mod2obspred'.}
  \item{event}{the value whose prevalence we want to calculate (e.g. 1, "present", etc.). This argument is ignored if 'model' is provided.}
  \item{na.rm}{logical, whether NA values should be excluded from the calculation. The default is TRUE.}
}
\value{
Numeric value of the prevalence of \code{event} in the \code{obs} vector.
}
\author{
A. Marcia Barbosa
}
\examples{

# calculate prevalence from binary vectors:

(x <- rep(c(0, 1), each = 5))

(y <- c(rep(0, 3), rep(1, 7)))

(z <- c(rep(0, 7), rep(1, 3)))

prevalence(x)

prevalence(y)

prevalence(z)


(w <- c(rep("yes", 3), rep("nope", 7)))

prevalence(w, event = "yes")


# calculate prevalence from a model object:

data(rotif.env)

mod <- glm(Abrigh ~ HabitatDiversity + HumanPopulation, family = binomial, data = rotif.env)

prevalence(model = mod)

# same as:
prevalence(obs = rotif.env$Abrigh)
}
