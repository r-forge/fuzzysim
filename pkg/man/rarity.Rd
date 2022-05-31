\name{rarity}
\alias{rarity}
\title{
(Fuzzy) rarity
}
\description{
This function computes the index of species rarity of Real et al. (2006), using either crisp (presence/absence, i.e. ones and zeros) or fuzzy values (e.g. \code{\link{Fav}}ourability, between zero and one), for a single species or for several species across a study area. Rarity is like a (potential) richness index in which rarer species have higher weight.
}
\usage{
rarity(data, sp.cols = 1:ncol(data), na.rm = TRUE)
}
\arguments{
  \item{data}{a numeric vector, matrix or data frame containing the presence/absence or the \code{\link{Fav}}ourability (fuzzy presence) values for the target species.}
  \item{sp.cols}{names or index numbers of the columns of 'data' that contain the species values for which to compute rarity. The default is to use all columns.}
  \item{na.rm}{logical value indicating whether NA values should be removed before the computation.}
}
\details{
If the input data include only one species (i.e. a numeric vector or a one-column table, with one value for each locality), rarity is 1 divided by the sum of its values. If the input includes more than one species or column, rarity is the sum of the product of each (fuzzy) presence value by the rarity of the corresponding species, so that rarer species have higher weight in the resulting sum (Real et al. 2006). See also Estrada et al. (2011) for a more complex version of fuzzy rarity.
}
\value{
If 'data' is a vector or a one-column table, or if 'sp.cols' is of length 1, this function returns a single value of rarity for the underlying species, which is simply 1 divided by the sum of its values. If 'data' and 'sp.cols' refer to more than 1 column, the function returns the total combined rarity value of all corresponding species for each row in 'data' (see Examples).
}
\references{
Real R., Estrada A., Barbosa A.M. & Vargas J.M. (2006) Aplicacion de la logica difusa al concepto de rareza para su uso en Gap Analysis: el caso de los mamiferos terrestres en Andalucia. Serie Geografica 13: 99-116

Estrada A., Real R. & Vargas J.M. (2011) Assessing coincidence between priority conservation areas for vertebrate groups in a Mediterranean hotspot. Biological Conservation, 144: 1120-1129
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{vulnerability}}
}
\examples{
data(rotif.env)

head(rotif.env)

rarity(rotif.env[ , 18])

rarity(rotif.env, sp.cols = "Abrigh")

rarity(rotif.env, sp.cols = 18:47)
# yields one value of combined rarity for each row in 'data'


# fuzzy rarity (from favourability values):

pred <- multGLM(rotif.env, sp.cols = 18:20, var.cols = 5:17)$predictions

head(pred)

rarity(pred, sp.cols = "Abrigh_F")

rarity(pred, sp.cols = c("Abrigh_F", "Afissa_F", "Apriod_F"))
# yields one value of combined rarity for each row in 'data'
}
