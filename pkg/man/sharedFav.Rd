\name{sharedFav}
\alias{sharedFav}
\title{
Shared favourability for two competing species
}
\description{
This function implements the graphical analyses of Acevedo et al. (2010, 2012) on biogeographical interactions. It takes two vectors of favourability values at different localities for, respectively, a stronger and a weaker competing species (or two equally strong competitors), and plots their favourableness or shared favourability to assess potential competitive interactions.
}
\usage{
sharedFav(strong_F, weak_F, conf = 0.95, bin_interval = "0.1", ...)
}
\arguments{
  \item{strong_F}{a numeric vector of favourability values (obtained, e.g., with functions \code{\link{Fav}} or \code{\link{multGLM}}) for the stronger species.}
  \item{weak_F}{a numeric vector of favourability values for the weaker species. Must be of the same length and in the same order as strong_F.}
  \item{conf}{confidence level for the confidence intervals in the plot. The default is 0.95. Set it to NA for no confidence intervals (see "Note" below).}
  \item{bin_interval}{character value specifying the method for grouping the favourability values into bins for plotting and comparing mean favourability for each species. Currently implemented options are "0.1" (the default, dividing the values at 0.1 intervals as per Acevedo et al. 2010, 2012) and "quantiles" (as the former method may produce an error if there are bins too small to allow computing confidence intervals). See "Note" below.}
  \item{\dots}{some additional arguments can be passed to \code{\link{barplot}}, such as "main" (for the plot title) or "las" (for the orientation of the axis labels).}
}
\details{
This function implements the biogeographic analyses of Acevedo et al. (2010, 2012), assessing the trends of environmental favourability across a range of favourability intersection values between two interacting species. It first calculates the fuzzy intersection (minimum value) between the two species' favourability values at each locality (i.e., favourability for the occurrence of both species simultaneously); it groups these values into 10 bins; and calculates the mean favourability (and its confidence interval) for each of the two species within each interval.

According to the notion of "favorableness" by Richerson & Lum (1980), competing species may or may not be able to coexist depending on their relative environmental fitnesses; competition between species increases and competitive exclusion decreases as their favourability intersection increases (Acevedo et al. 2010, 2012). The shaded area in the shared favourability plot, where at least one of the species is at intermediate favourability, is the area where competitive interactions may limit species occurrence. Outside this shaded area, where favourability is either very low for at least one of the species (left) or very high for both species (right side of the plot), competition is not limiting (see also \code{\link{bioThreat}} for details).
}
\value{
This function returns the numeric value of the fuzzy overlap index (FOvI; Dubois & Prade 1980, Acevedo et al. 2010, 2012), a data frame with the bin values and the shared favourability plot, with circles and a continuous line representing favourability for the stronger species, and squares and a dashed line representing favourability for the weaker species. The height of the bars at the bottom represents the proportional sample size of each bin.
}
\note{
This function may generate an error if one or more bins don't have enough values for the confidence interval to be computed. If this occurs, you can try a different 'bin_interval' (e.g. "quantiles") or set the 'conf' argument to NA (in which case confidence intervals will not be computed). Either will affect only the plot, not the overall fuzzy overlap value.
}
\references{
Acevedo P., Ward A.I., Real R. & Smith G.C. (2010) Assessing biogeographical relationships of ecologically related species using favourability functions: a case study on British deer. Diversity and Distributions, 16: 515-528

Acevedo P., Jimenez-Valverde A., Melo-Ferreira J., Real R. & Alves, P.C. (2012) Parapatric species and the implications for climate change studies: a case study on hares in Europe. Global Change Biology, 18: 1509-1519

Dubois D. & Prade H. (1980) Fuzzy sets and systems: theory and applications. Academic Press, New York

Richerson P.J. & Lum K. (1980) Patterns of plant species and diversity in California: relation to weather and topography. American Naturalist, 116: 504-536
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
sharedFav(strong_F = favs[,1], weak_F = favs[,2], main = "Shared favourability")

sharedFav(strong_F = favs[,1], weak_F = favs[,2], bin_interval = "quantiles",
main = "Shared favourability", las = 2)
}
