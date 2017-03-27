\name{integerCols}
\alias{integerCols}
\title{
Classify integer columns
}
\description{
This function detects which numeric columns in a data frame contain only whole
numbers, and converts those columns to integer class, so that they take up
less space.
}
\usage{
integerCols(data)
}
\arguments{
  \item{data}{a data frame containing possibly integer columns classified as \code{numeric}.}
}
\value{
The function returns a data frame with the same columns as \code{data}, but with
those that are numeric and contain only whole numbers (possibly including NA)
now classified as \code{integer}.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{is.integer}}, \code{\link{as.integer}}, \code{\link{multConvert}}
}
\examples{
dat <- data.frame(
  var1 = 1:10,
  var2 = as.numeric(1:10),
  var3 = as.numeric(c(1:4, NA, 6:10)),
  var4 = as.numeric(c(1:3, NaN, 5, Inf, 7, -Inf, 9:10)),
  var5 = as.character(1:10),
  var6 = seq(0.1, 1, by = 0.1),
  var7 = letters[1:10]
)  # creates a sample data frame

dat

str(dat)
# var2 classified as 'numeric' but contains only whole numbers
# var3 same as var2 but containing also NA values
# var4 same as var2 but containing also NaN and infinite values
# var5 contains only whole numbers but initially classified as factor

dat <- integerCols(dat)

str(dat)
# var2 and var3 now classified as 'integer'
# var4 remains as numeric because contains infinite and NaN 
# (not integer) values
# var5 remains as factor
}
\keyword{ classes }
\keyword{ manip }
