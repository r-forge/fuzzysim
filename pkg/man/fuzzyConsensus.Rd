\name{fuzzyConsensus}
\alias{fuzzyConsensus}
\title{Fuzzy consensus among model predictions}
\description{This function takes a data frame or a (multilayer) SpatRaster map of favourability predictions (i.e., directly comparable predictions obtained from presence probability; see \code{\link{Fav}}) and it computes the consensus favourability, i.e., a row-wise weighted mean in which larger weights are assigned to models with higher loadings in the first axis of a principal components analysis (Baquero et al. 2021).}
\usage{fuzzyConsensus(data, weights = "PCA1", simplif = TRUE, plot = TRUE, 
biplot = FALSE, verbosity = 2, do.par = TRUE)}
\arguments{
  \item{data}{matrix, data frame or (multilayer) 'SpatRaster' map containing the favourability values to combine.}
  \item{weights}{method for computing the weights for the weighted average of favourability values. Currently only "PCA1" is implemented.}
  \item{simplif}{logical value. If TRUE (the default), the output includes only the numeric vector of weighted mean favourability. If set to FALSE, the output will include also the complete PCA result (if weights="PCA1").}
  \item{plot}{logical value indicating whether to produce a barplot of the PCA axis loadings. The default is TRUE.}
  \item{biplot}{logical value indicating whether to produce a \code{\link{biplot}} of the PCA. The default is FALSE, as it makes computation slower.}
  \item{verbosity}{integer value indicating the amount of messages to display in the console. The default is to emit all messages available.}
  \item{do.par}{logical value indicating whether to override the current plotting parameters (restoring them on exit). The default is TRUE.}
}
\details{
Species distribution models are often computed using different modelling methods and/or climate scenarios. One way to summarize or combine them is to do a principal components analysis (PCA) of the different model predictions: The first axis of this PCA captures consistent spatial patterns in the predicted values across the different models (Araujo, Pearson, et al. 2005; Araujo, Whittaker, et al. 2005; Marmion et al. 2009; Thuiller 2004). However, the units of the PCA axes are difficult to interpret. Baquero et al. (2021) solved this by computing a weighted average of the favourability values (which are commensurable and therefore directly comparable across species and study areas; Real et al. 2006, Acevedo & Real 2012), using the loadings of the first PCA axis as weights. The result is therefore in the same scale as favourability, and it incorporates the degree of consensus among models, which dictates how much weight each model has in the prediction, thus avoiding disparate predictions to be blindly mixed and averaged out (Baquero et al. 2021).
}
\value{
If simplif=TRUE (the default), the function returns a numeric vector with length equal to the number of rows in 'data' (if 'data' is a matrix or data frame), or a 'SpatRaster' layer (if 'data' is a 'SpatRaster' object), with the consensus among the input favourabilities. If simplif=FALSE, the function returns a list containing, additionally, the output of \code{\link{prcomp}}.
}
\references{
Acevedo P. & Real R. (2012) Favourability: Concept, distinctive characteristics and potential usefulness. Naturwissenschaften, 99: 515-522

Araujo M.B., Pearson R.G., Thuiller W. & Erhard M. (2005) Validation of species-climate impact models under climate change. Global Change Biology, 11: 1504-1513

Araujo M.B., Whittaker R.J., Ladle R.J. & Erhard M. (2005) Reducing uncertainty in projections of extinction risk from climate change. Global Ecology and Biogeography, 14: 529-538

Baquero R.A., Barbosa A.M., Ayllon D., Guerra C., Sanchez E., Araujo M.B. & Nicola G.G. (2021) Potential distributions of invasive vertebrates in the Iberian Peninsula under projected changes in climate extreme events. Diversity and Distributions, 27(11): 2262-2276

Marmion M., Parviainen M., Luoto M., Heikkinen R.K. & Thuiller W. (2009) Evaluation of consensus methods in predictive species distribution modelling. Diversity and Distributions 15: 59-69

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245

Thuiller W. (2004) Patterns and uncertainties of species' range shifts under climate change. Global Change Biology, 10: 2020-2027
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{weighted.mean}}
}
\examples{
\dontrun{
# this example requires having the 'gam' package installed

data(rotif.env)

library(gam)


# get two different model predictions for one of the species in this dataset:

names(rotif.env)
vars <- names(rotif.env)[5:17]

form_glm <- as.formula(paste("Ttetra ~", paste(vars, collapse = "+")))
mod_glm <- glm(form_glm, family = binomial, data = rotif.env)
pred_glm <- predict(mod_glm, rotif.env, type = "response")

form_gam <- as.formula(paste("Ttetra ~", paste("s(", vars, ")", collapse = "+")))
mod_gam <- gam(form_gam, family = binomial, data = rotif.env)
pred_gam <- predict(mod_gam, rotif.env, type = "response")


# convert probability predictions to favourability:

fav_glm <- Fav(pred = pred_glm, sample.preval = prevalence(model = mod_glm))
fav_gam <- Fav(pred = pred_gam, sample.preval = prevalence(model = mod_gam))


# compute the consensus favourability of these two models:

fav_consensus <- fuzzyConsensus(cbind(fav_glm, fav_gam))

cor(cbind(fav_glm, fav_gam, fav_consensus))
}
}
