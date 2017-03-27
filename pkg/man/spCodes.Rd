\name{spCodes}
\alias{spCodes}
\title{
Obtain unique abbreviations of species names
}
\description{
This function takes a vector of species names and converts them to abbreviated species codes containing the specified numbers of characters from the genus, the specific and optionally also the subspecific name. Separators can be specified by the user. The function checks that the resulting codes are unique.
}
\usage{
spCodes(species, nchar.gen = 3, nchar.sp = 3, nchar.ssp = 0, 
sep.species = " ", sep.spcode = "")
}
\arguments{
  \item{species}{
a character vector containig the species names to be abbreviated.
}
  \item{nchar.gen}{
the number of characters from the genus name to be included in the resulting species code.
}
  \item{nchar.sp}{
the number of characters from the specific name to be included in the resulting species code.
}
  \item{nchar.ssp}{
optionally, the number of characters from the subspecific name to be included in the resulting species code. Set it to 0 if you have subspecific names in \code{species} but do not want them included in the resulting species codes.
}
  \item{sep.species}{
the character that separates genus, specific and subspecific names in \code{species}. The default is a white space.
}
  \item{sep.spcode}{
the character you want separating genus and species abbreviations in the resulting species codes. The default is an empty character (no separator).
}
}
\value{
This function returns a character vector containing the species codes resulting from the abbreviation. If the numbers of characters specified do not make for unique codes, an error message is displayed showing which \code{species} names caused it, so that you can try again with different \code{nchar.gen}, \code{nchar.sp} and/or
\code{nchar.ssp}.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{substr}}, \code{\link{strsplit}}
}
\examples{
data(rotifers)

head(rotifers)

## add a column to 'rotifers' with shorter versions of the species names:

\dontrun{
rotifers$spcode <- spCodes(rotifers$species, sep.species = "_", 
nchar.gen = 1, nchar.sp = 4, nchar.ssp = 0, sep.spcode = ".")

# this produces an error due to resulting species codes not being unique
}

rotifers$spcode <- spCodes(rotifers$species, sep.species = "_", 
nchar.gen = 1, nchar.sp = 5, nchar.ssp = 0, sep.spcode = ".")

# with a larger number of characters from the specific name,
# resulting codes are now unique

## check out the result:
head(rotifers)
}
\keyword{ character }
