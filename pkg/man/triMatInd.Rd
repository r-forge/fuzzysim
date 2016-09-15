\name{triMatInd}
\alias{triMatInd}
\title{
Triangular matrix indices
}
\description{
This function outputs the indices of one triangle (the lower one by default) of an input square matrix. It is used by \code{\link{simMat}} and, for large matrices, makes it faster than e.g. with \code{\link{lower.tri}} or \code{\link{upper.tri}}.
}
\usage{
triMatInd(mat, lower = TRUE, list = FALSE)
}
\arguments{
  \item{mat}{
a square matrix.
}
  \item{lower}{
logical indicating whether the indices should correspond to the lower triangle. The default is TRUE; FALSE produces the upper triangle indices.
}
  \item{list}{
logical indicating whether the results should be output as a list instead of a matrix. The default is FALSE.
}
}
\value{
The indices (row, column) of the elements of the matrix that belong to the requested triangle.
}
\references{
http://stackoverflow.com/questions/20898684/how-to-efficiently-generate-lower-triangle-indices-of-a-symmetric-matrix
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{lower.tri}}, \code{\link{upper.tri}}
}
\examples{
mat <- matrix(nrow = 4, ncol = 4)
mat
triMatInd(mat)
triMatInd(mat, list = TRUE)
}
