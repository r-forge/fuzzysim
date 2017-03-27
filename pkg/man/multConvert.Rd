\name{multConvert}
\alias{multConvert}
\title{
Multiple conversion
}
\description{
This function can simultaneously convert multiple columns of a matrix or data frame.
}
\usage{
multConvert(data, conversion, cols = 1:ncol(data))
}
\arguments{
  \item{data}{A matrix or data frame containing columns that need to be converted}
  \item{conversion}{the conversion to apply, e.g. \code{as.factor} or a custom-made fucntion}
  \item{cols}{the columns of \code{data} to convert}
}
\details{
Sometimes we need to change the data type (class, mode) of a variable in R. There are various possible conversions, performed by functions like as.integer, as.factor or as.character. If we need to perform the same conversion on a number of variables (columns) in a data frame, we can convert them all simultaneously using this function. By default it converts all the columns in the data frame, but you can specify just a few of them. multConvert can also be used to apply other kinds of transformations - for example, if you need to divide some of your columns by 100, just write a function to do this and then use multConvert to apply this function to any group of columns.
}
\value{The input data with the specified columns converted as asked.}
\author{
A. Marcia Barbosa}
\examples{
data(rotif.env)

str(rotif.env)

# convert the first 4 columns to character:
converted.rotif.env <- multConvert(data = rotif.env, 
conversion = as.character, cols = 1:4)

str(converted.rotif.env)


names(rotif.env)

# divide some columns by 100:

div100 <- function(x) x / 100

rotif.env.cent <- multConvert(data = rotif.env, 
conversion = div100, cols = c(6:10, 12:17))

head(rotif.env.cent)
}
\keyword{ manip }
\keyword{ classes }
