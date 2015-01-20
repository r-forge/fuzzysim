\name{prevalence}
\alias{prevalence}
\title{
Prevalence
}
\description{
For building and evaluating species distribution models, the porportion of presences of the species may be issues to take into account (e.g. Jimenez-Valverde & Lobo 2006, Barbosa et al. 2013). The \code{prevalence} function calculates this measure.
}
\usage{
prevalence(obs, event = 1)
}
\arguments{
  \item{obs}{a vector of binary observations (e.g. 1 vs. 0, male vs. female, disease vs. no disease, etc.).}
  \item{event}{the value whose prevalence we want to calculate (e.g. 1, "present", etc.).}
}
\details{
}
\value{
Numeric value of the prevalence of \code{event} in the \code{obs} vector.
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions, in press

Jimenez-Valverde A. & Lobo J.M. (2006) The ghost of unbalanced species distribution data in geographical model predictions. Diversity and Distributions, 12: 521-524.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{evenness}}
}
\examples{
(x <- rep(c(0, 1), each = 5))

(y <- c(rep(0, 3), rep(1, 7)))

(z <- c(rep(0, 7), rep(1, 3)))

prevalence(x)

prevalence(y)

prevalence(z)
}
\keyword{ binary data }
