\name{multConvert}
\alias{multConvert}
\title{
Multiple conversion
}
\description{
This function can simultaneously convert multiple columns of a matrix or data frame, or multiple layers of a SpatRaster.
}
\usage{
multConvert(data, conversion, cols = 1:ncol(data))
}
\arguments{
  \item{data}{A matrix, an object inheriting class data frame, or a SpatRaster containing the columns or layers that need to be converted}
  \item{conversion}{the conversion to apply, e.g. \code{\link{as.factor}}, \code{\link{scale}}, \code{\link{log}}, or a custom-made function}
  \item{cols}{the columns or layers of 'data' to convert}
}
\details{
With this function we can change the data type (e.g. convert with \code{\link{as.integer}}, \code{\link{as.factor}} or \code{\link{as.character}}), \code{\link{scale}} or \code{\link{log}}-transform several variables in a data frame. By default, all columns in 'data' are converted, but you can specify just some of those in the 'cols' argument. You can also specify your own function to apply, e.g. a division/multiplication of several columns by a given number (see Examples).
}
\value{The input data with the specified columns converted as specified in 'conversion'.}
\author{A. Marcia Barbosa}
\examples{
data(rotif.env)

str(rotif.env)


# convert the first 4 columns to character:

converted.rotif.env <- multConvert(data = rotif.env,
conversion = as.character, cols = 1:4)

str(converted.rotif.env)


# divide some columns by 100:

div100 <- function(x) {
  x / 100
}

rotif.env.div100 <- multConvert(data = rotif.env,
conversion = div100, cols = c(6:10, 12:17))

head(rotif.env.div100)


# scale (standardize) continuous variables:

names(rotif.env)

conts <- names(which(sapply(rotif.env[ , 1:17], is.numeric)))

rotif.env.scaled <- multConvert(data = rotif.env,
conversion = scale, cols = conts)

head(rotif.env.scaled)
}
\keyword{ manip }
\keyword{ classes }
