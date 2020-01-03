\name{bioThreat}
\alias{bioThreat}
\title{
Biotic threat of a stronger over a weaker species based on their favourability values
}
\description{
This function takes two vectors of \code{\link{Fav}}ourability values at different localities for, respectively, a stronger and a weaker species (e.g., a superior vs. an inferior competitor, or an invasive predator vs. an unadapted native prey), and calculates the level of threat that the former may potentially pose to the latter in each locality.
}
\usage{
bioThreat(strong_F, weak_F, character = FALSE, ...)
}
\arguments{
  \item{strong_F}{a numeric vector of favourability values (obtained, e.g., with functions \code{\link{Fav}} or \code{\link{multGLM}}) for the stronger species.}
  \item{weak_F}{a numeric vector of favourability values for the weaker species. Must be of the same lenght and in the same order as strong_F.}
  \item{character}{logical value indicating whether the result should be returned in character rather numeric form. Defaults to FALSE.}
  \item{\dots}{additional arguments to pass to \code{\link{favClass}}, namely the breaks for separating favourability values into low, intermediate and high (see Details).}
}
\details{
Based on the notion of "favorableness" by Richerson & Lum (1980), according to which competing species may or may not be able to coexist depending on their relative environmental fitnesses, Acevedo et al. (2010, 2012) and some subsequent studies (e.g. Romero et al. 2014, Munoz et al. 2015, Chamorro et al. 2019) proposed possible biotic interaction outcomes of different combinations of favourability values for two species. Favourability has the advantage, in contrast with other types of potential distribution metrics, of being directly comparable among diferent species, independently of their relative occurrence frequencies (see \code{\link{Fav}}). This function builds on those proposals by including additional possible combinations of higher, intermediate or low favourability values (following Munoz & Real 2006), producing the following classification of biotic threat across a set of analysed localities:

0 ('white'): areas where favourability is low for at least one of the species (abiotic exclusion), so biotic threat does not apply.

1 ('green'): areas where favourability is high for both species, so they should be able to co-ocur (sympatric coexistence) and biotic threat is low.

2 ('yellow'): areas where favourability is high for the weaker species and intermediate for the stronger species, so the level of threat is moderate.

3 ('orange'): areas where favourability is intermediate for both species, so the stronger one potentially prevails and the level of threat is high.

4 ('red'): areas where favourability is high for the stronger species and intermediate for the weaker species, in which case the level of threat is very high (biotic exclusion).
}
\value{
This function returns either an integer or a character vector (following the 'character' argument, which is set to FALSE by default) of the same length as strong_F and weak_F, classifying each locality with the level of biotic threat posed by the former on the latter (see Details).
}
\references{
Acevedo P., Ward A.I., Real R. & Smith G.C. (2010) Assessing biogeographical relationships of ecologically related species using favourability functions: a case study on British deer. Diversity and Distributions, 16: 515-528

Acevedo P., Jimenez-Valverde A., Melo-Ferreira J., Real R. & Alves, P.C. (2012) Parapatric species and the implications for climate change studies: a case study on hares in Europe. Global Change Biology, 18: 1509-1519

Chamorro D., Munoz A.R., Martinez-Freiria F. & Real R. (2019) Using the fuzzy logic in the distribution modelling of competitive interactions. Poster, IBS Malaga 2019 - 9th Biennial Conference of the International Biogeography Society

Munoz A.R. & Real R. (2006) Assessing the potential range expansion of the exotic monk parakeet in Spain. Diversity and Distributions, 12: 656-665

Munoz A.R., Real R. & Marquez A.L. (2015) Interacciones a escala nacional entre rapaces rupicolas en base a modelos de distribucion espacial. Los casos del buitre leonado, alimoche y aguila perdicera. Informe tecnico, Universidad de Malaga & Fundacion EDP

Richerson P.J. & Lum K. (1980) Patterns of plant species diversity in California: relation to weather and topography. American Naturalist, 116:504-536

Romero D., Baez J.C., Ferri-Yanez F., Bellido J. & Real R. (2014) Modelling favourability for invasive species encroachment to identify areas of native species vulnerability. The Scientific World Journal, 2014: 519710
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{sharedFav}}, \code{\link{Fav}}, \code{\link{favClass}}
}
\examples{
data(rotif.env)
mods <- multGLM(rotif.env, sp.cols = 19:20, var.cols = 5:17)
head(mods$predictions)
favs <- mods$predictions[ , 3:4]
threat <- bioThreat(strong_F = favs[,1], weak_F = favs[,2])
threat_chr <- bioThreat(strong_F = favs[,1], weak_F = favs[,2], char = TRUE)
data.frame(favs, threat = threat, threat_col = threat_chr)
}
