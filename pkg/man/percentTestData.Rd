\name{percentTestData}
\alias{percentTestData}
\title{
Percent test data
}
\description{
Based on the work of Schaafsma & van Vark (1979), Huberty (1994) provided a heuristic ("rule of thumb") for determining an adequate proportion of data to set aside for testing species presence/absence models, based on the number of predictor variables that are used (Fielding & Bell 1997). The \code{percentTestData} function calculates this proportion as a percentage.
}
\usage{
percentTestData(nvar)
}
\arguments{
  \item{nvar}{the number of variables in the model.}
}
\value{
A numeric value of the percentage of data to leave out of the model for further model testing.
}
\references{
Huberty C.J. (1994) Applied Discriminant Analysis. Wiley, New York, 466 pp.

Schaafsma W. & van Vark G.N. (1979) Classification and discrimination problems with applications. Part IIa. Statistica Neerlandica 33: 91-126

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{multGLM}}}
\examples{
# say you're building a model with 15 variables:

percentTestData(15)

# the result tells you that 21\% is an appropriate percentage of data
# to set aside for testing your model, so train it with 79\% of the data
}
\keyword{ models }
