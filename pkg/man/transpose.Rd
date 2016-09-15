\name{transpose}
\alias{transpose}
\title{
Transpose (part of) a matrix or dataframe
}
\description{
This function transposes (a specified part of) a matrix or data frame, optionally using one of its columns as column names for the transposed result. It can be useful for turning a species presence-absence table into a regional species composition table.
}
\usage{
transpose(data, sp.cols = 1:ncol(data), reg.names = NULL)
}
\arguments{
  \item{data}{
a matrix or data frame containing the species occurrence data to transpose.
}
  \item{sp.cols}{
names or index numbers of the columns containing the species occurrences in \code{data} which are meant to be transposed.
}
  \item{reg.names}{
name or index number of the column in \code{data} containing the region names, to be used as column names in the transposed result.
}
}
\value{
This function returns the transposed \code{sp.cols} of \code{data}, with the column specified in \code{reg.names} as column names.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{t}}
}
\examples{
data(rotif.env)

head(rotif.env)

names(rotif.env)

rotif.reg <- transpose(rotif.env, sp.cols = 18:47, reg.names = 1)

head(rotif.reg)
}
\keyword{ manip }
