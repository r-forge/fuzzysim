\name{timer}
\alias{timer}
\title{Timer}
\description{
Reporting of time elapsed since a given start time, or during the running of an expression.
}
\usage{
timer(start.time, digits = 1)
}
\arguments{
  \item{start.time}{A date-time object of class \code{\link{POSIXct}}, e.g. as given by \code{\link{Sys.time}}; or an expression to be timed.}
  \item{digits}{integer value specifying the number of decimal places to \code{\link{round}} the output to.}
}
\value{
The function returns a message informing of the time elapsed since the input 'start.time', or during the running of the input expression.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{Sys.time}}, \code{\link{proc.time}}, \code{\link{difftime}}
}
\examples{
# get starting time:
start <- Sys.time()

# do some random analysis:
x <- sort(rnorm(3e7))

# see how long it took:
timer(start)


# time an expression directly:
timer(x <- sort(rnorm(3e7)))
timer(x <- sort(rnorm(3e7)), digits = 2)
}
