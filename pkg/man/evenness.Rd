\name{evenness}
\alias{evenness}
\title{
Evenness in a binary vector.
}
\description{
For building and evaluating species distribution models, the porportion of presences (prevalence) of a species and the balance between the number of presences and absences may be issues to take into account (e.g. Jimenez-Valverde & Lobo 2006, Barbosa et al. 2013). The \code{evenness} function calculates the presence-absence balance in a binary (e.g., presence/absence) vector.
}
\usage{
evenness(obs)
}
\arguments{
  \item{obs}{
a vector of binary observations (e.g. 1 or 0, male or female, disease or no disease, etc.)
  }
}
\details{
}
\value{
A number ranging between 0 when all values are the same, and 1 when there are the same number of cases with each value in \code{obs}.
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions, 19: 1333-1338

Jimenez-Valverde A. & Lobo J.M. (2006) The ghost of unbalanced species distribution data in geographical model predictions. Diversity and Distributions, 12: 521-524.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{prevalence}}
}
\examples{
(x <- rep(c(0, 1), each = 5))
(y <- c(rep(0, 3), rep(1, 7)))
(z <- c(rep(0, 7), rep(1, 3)))

prevalence(x)
evenness(x)

prevalence(y)
evenness(y)

prevalence(z)
evenness(z)
}
\keyword{ binary data }
