\name{favClass}
\alias{favClass}
\title{
Classify favourability into 3 categories (low, intermediate, high)
}
\description{
This function takes a vector of \code{\link{Fav}}ourability values and reclassifies them into 3 increasing categories: low, intermediate or high. By default, the breaks between these classes are 0.2 and 0.8 (see Details), although these can be changed by the user.
}
\usage{
favClass(fav, breaks = c(0.2, 0.8), character = FALSE)
}
\arguments{
  \item{fav}{a numeric vector of favourability values (obtained, e.g., with functions \code{\link{Fav}} or \code{\link{multGLM}}).}
  \item{breaks}{a numeric vector of length 2 containing the two values which will divide \code{fav} into the 3 classes. Defaults to c(0.2, 0.8) following the literature (see Details).}
  \item{character}{logical value (default FALSE) indicating whether the result should be returned in character rather numeric form.}
}
\details{
Some applications of species distribution models imply setting a threshold to separate areas with high and low probability or favourability for occurrence (see, e.g., \code{\link{bioThreat}}). However, it makes little sense to establish as markedly different areas with, for example, 0.49 and 0.51 favourability values (Hosmer & Lemeshow, 1989). It may thus be wiser to open a gap between values considered as clearly favourable and clearly unfavourable. When this option is taken in the literature, commonly used breaks are 0.8 as a threshold to classify highly favourable values, as the odds are more than 4:1 favourable to the species; 0.2 as a threshold below which to consider highly unfavourable values, as odds are less than 1:4; and classifying the remaining values as intermediate favourability (e.g., Munoz & Real 2006, Olivero et al. 2016).
}
\value{
This function returns either an integer or a character vector (depending on the 'character' argument, which is FALSE by default), of the same length as \code{fav}, reclassifying it into 3 categories: 1 ('low'), 2 ('intermediate'), or 3 ('high').
}
\references{
Hosmer D.W. Jr & Lemeshow S. (1989) Applied logistic regression. John Wiley & Sons, New York

Munoz A.R. & Real R. (2006) Assessing the potential range expansion of the exotic monk parakeet in Spain. Diversity and Distributions, 12: 656-665

Olivero J., Fa J.E., Real R., Farfan M.A., Marquez A.L., Vargas J.M., Gonzalez J.P., Cunningham A.A. & Nasi R. (2017) Mammalian biogeography and the Ebola virus in Africa. Mammal Review, 47: 24-37
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{Fav}}, \code{\link{multGLM}}
}
\examples{
data(rotif.env)
mods <- multGLM(rotif.env, sp.cols = 20, var.cols = 5:17)
fav <- mods$predictions[ , 2]
data.frame(fav = fav, favcl_num = favClass(fav),
favcl_chr = favClass(fav, character = TRUE))
}
