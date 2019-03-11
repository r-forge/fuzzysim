\name{sharedFav}
\alias{sharedFav}
\title{
Shared favourability for two competing species
}
\description{
This function implements the graphical analyses of Acevedo et al. (2010, 2012) on biogeographical interactions. It takes two vectors of favourability values at different localities for, respectively, a stronger and a weaker competing species (or two equally strong competitors), and plots their favourableness or shared favourability to assess potential competitive interactions.
}
\usage{
sharedFav(strong_F, weak_F, conf = 0.95, main = "Shared favourability")
}
\arguments{
  \item{strong_F}{a numeric vector of favourability values (obtained, e.g., with functions \code{\link{Fav}} or \code{\link{multGLM}}) for the stronger species.}
  \item{weak_F}{a numeric vector of favourability values for the weaker species. Must be of the same lenght and in the same order as strong_F.}
    \item{conf}{Confidence level for the confidence intervals in the plot. Defaults to 0.95.}
    \item{main}{Character indicating a title for the plot.}
}
\details{
This function implements biogeographic analyses of Acevedo et al. (2010, 2012) assessing the trends of environmental favourability across a range of favourability intersection values between two competing species. It first calculates the fuzzy intersection (minimum value) between the two species' favourability values at each locality (i.e., the favourability for occurrence of at least one of the species); it groups these values into 10 bins of width 0.1; and calculates the mean favourability (together with the confidence interval) within each interval for each of two species.

According to the notion of "favorableness" by Richerson & Lum (1980), competing species may or may not be able to coexist depending on their relative environmental fitnesses; competition between species increases and competitive exclusion decreases as their favourability intersection increases (Acevedo et al., 2010, 2012). The shaded area in the shared favourability plot, where at least one of the species is at intermediate favourability, is the area where competitive interactions may limit species occurrence. Outside this shaded area, where favourability is either very low for at least one of the species (left) or very high for both species (right side of the plot), competition is not limiting (see also \code{\link{bioThreat}} for details).
}
\value{
This function provides the shared favourability plot, with circles and a continuous line representing favourability for the stronger species, and squares and a dashed lines representing favourability for the weaker species; and it returns the numeric value of the fuzzy overlap index (Acevedo et al., 2010, 2012).
}
\references{
Acevedo P., Ward A.I., Real R. & Smith G.C. (2010) Assessing biogeographical relationships of ecologically related species using favourability functions: a case study on British deer. Diversity and Distributions, 16: 515-528

Acevedo P., Jimenez-Valverde A., Melo-Ferreira J., Real R. & Alves, P.C. (2012) Parapatric species and the implications for climate change studies: a case study on hares in Europe. Global Change Biology, 18: 1509-1519

Richerson P.J. & Lum K. (1980) Patterns of plant species and diversity in California: relation to weather and topography. American Naturalist 116: 504-536
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{bioThreat}}, \code{\link{Fav}}
}
\examples{
# get favourability model predictions for two species:
data(rotif.env)
mods <- multGLM(rotif.env, sp.cols = 19:20, var.cols = 5:17)
head(mods$predictions)
favs <- mods$predictions[ , 3:4]

# get shared favourability:
sharedFav(strong_F = favs[,1], weak_F = favs[,2])
}
