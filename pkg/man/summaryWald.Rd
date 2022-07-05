\name{summaryWald}
\alias{summaryWald}
\title{
Model summary with Wald (instead of z) test statistics
}
\description{
This function produces a summary of a generalized linear model, with the Wald test (instead of the z test) and associated statistics.
}
\usage{
summaryWald(model, interceptLast = TRUE)
}
\arguments{
  \item{model}{
a model object of class "glm".
}
  \item{interceptLast}{
logical, whether to place the intercept in the last (rasther than the first) row of the output. Defaults to TRUE.
}
}
\details{
This function requires the \pkg{aod} package, whose \code{wald.test} function is used for computing the Wald test.
}
\value{
This function returns a data frame with the model summary statistics.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{summary}}
}
\examples{
# load sample data:

data(rotif.env)
names(rotif.env)


# build a model of a species' occurrence based on 
# some of the variables:

model <- glm(Abrigh ~ Area + Altitude + AltitudeRange + HabitatDiversity + 
HumanPopulation, family = binomial, data = rotif.env)


# get the Wald-based model summary:

summaryWald(model)
}
