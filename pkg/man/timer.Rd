\name{timer}
\alias{timer}
\title{Timer}
\description{
Reporting of time elapsed since a given start time. This function is used internally by other functions in the package.
}
\usage{
timer(start.time)
}
\arguments{
  \item{start.time}{A date-time object of class \code{\link{POSIXct}}, e.g. as given by \code{\link{Sys.time}}.}
}
\value{
The function returns a message informing of the time elapsed since the input 'start.time'.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{Sys.time}}, \code{\link{proc.time}}, \code{\link{difftime}}
}
\examples{
# get starting time:
start <- Sys.time()

# do some random analysis:
sapply(rnorm(50000), function(x) x*5)

# see how long it took:
timer(start)
}
