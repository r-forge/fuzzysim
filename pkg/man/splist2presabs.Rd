\name{splist2presabs}
\alias{splist2presabs}
\title{
Convert a species list to a presence-absence table
}
\description{
This function takes a locality+species dataset in long (stacked) format, i.e., a matrix or data frame containing localities in one column and their recorded species in another column, and converts them to a presence-absence table (wide format) suitable for mapping and for computing distributional similarities (see e.g. \code{\link{simMat}}). Try out the Examples below for an illustration).
}
\usage{
splist2presabs(data, sites.col, sp.col, keep.n = FALSE)
}
\arguments{
  \item{data}{
a matrix or data frame with localities in one column and species in another column. Type \code{data(rotifers); head(rotifers)} for an example.
}
  \item{sites.col}{
the name or index number of the column containing the localities in \code{data}.
}
  \item{sp.col}{
 the name or index number of the column containing the species names or codes in  \code{data}.
}
  \item{keep.n}{
logical value indicating whether to get in the resulting table the number of times each species appears in each locality; if \code{FALSE} (the default), only presence (1) or absence (0) is recorded.
}
}
\value{
A data frame containing the localities in the first column and then one column per species indicating their presence (or their number of records if \code{keep.n = TRUE)} and absence. Type \code{data(rotif.env); 
head(rotif.env[,18:47])} for an example.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{table}}
}
\examples{
data(rotifers)

head(rotifers)

rotifers.presabs <- splist2presabs(rotifers, sites.col = "TDWG4",
sp.col = "species", keep.n = FALSE)

head(rotifers.presabs)
}
\keyword{ manip }
