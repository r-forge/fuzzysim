\name{Fav}
\alias{Fav}
\title{
Favourability
}
\description{
Environmental (prevalence-independent) favourability for a species' presence
}
\usage{
Fav(model = NULL, obs = NULL, pred = NULL, n1n0 = NULL, 
sample.preval = NULL, method = "RBV", true.preval = NULL, 
verbosity = 2)
}
\arguments{
  \item{model}{
a model object of class "glm" and binomial family.
}
  \item{obs}{
a vector of the 1 and 0 values of the modelled binary variable (e.g. presence-absence of a species) in the modelled dataset or sample. This argument is ignored if 'model' is provided.
}
  \item{pred}{
a numeric vector or a RasterLayer of predicted presence probability values, given by any presence-absence modelling method. This argument is ignored if 'model' is provided.
}
  \item{n1n0}{
alternatively to 'obs', an integer vector of length 2 providing the total numbers of modelled ones and zeros, in this order. Ignored if 'obs' or 'model' is provided.
}
  \item{sample.preval}{
alternatively to 'obs' or 'n1n0', the prevalence (proportion of positive cases) of the modelled binary variable in the modelled dataset or sample. Ignored if 'model' is provided.
}
  \item{method}{
either "RBV" for the original Real, Barbosa & Vargas (2006) procedure, or "AT" if you want to try out the modification proposed by Albert & Thuiller (2008) (but see Details!).
  }
  \item{true.preval}{
the true prevalence (as opposed to sample prevalence), necessary if you want to use the "AT" method.
}
  \item{verbosity}{numeric value indicating the amount of messages to display; currently meaningful values are 0, 1, and 2 (the default).
  }
}
\details{
Methods such as Generalized Linear Models, Generalized Additive Models, Random Forests, Boosted Regression Trees / Generalized Boosted Models, Bayesian Additive Regression Trees and several others, are widely used for modelling species' potential distributions using presence/absence data and a set of predictor variables. These models predict presence probability, which incorporates the prevalence (proportion of presences) of the species in the modelled sample. So, predictions for restricted species are always generally low, while predictions for widespread species are always generally higher, regardless of the actual environmental quality. Barbosa (2006) and Real, Barbosa & Vargas (2006) proposed an environmental favourability function which is based on presence probability and cancels out uneven proportions of presences and absences in the modelled data. Favourability thus assesses the extent to which the environmental conditions change the probability of occurrence of a species with respect to its overall prevalence in the study area. Model predictions become, therefore, directly comparable among species with different prevalences. The favourability function is implemented in the \pkg{fuzzySim} package and is also in the SAM (Spatial Analysis in Macroecology) software (Rangel et al. 2010).

Using simulated data, Albert & Thuiller (2008) proposed a modification to the favourability function, but it requires knowing the true prevalence of the species (not just the prevalence in the modelled sample), which is rarely possible in real-world modelling. Besides, this suggestion was based on the misunderstanding that the favourability function was a way to obtain the probability of occurrence when prevalence differs from 50\%, which is incorrect (see Acevedo & Real 2012).

To get environmental favourability with either the Real, Barbosa & Vargas ("RBV") or the Albert & Thuiller ("AT") method, you just need to get model predictions of presence probability from your data, together with the proportions of presences and absences in the modelled sample, and then use the 'Fav' function. Input data for this function are either a model object resulting from the \code{\link{glm}} function, or the vector of presences-absences (1-0) of your species and the corresponding presence probability values, obtained e.g. with predict(mymodel, mydata, type = "response"). Alternatively to the presences-absences, you can provide either the sample prevalence or the numbers of presences and absences in the dataset that was used to generate the presence probabilities. In case you want to use the "AT" method, you also need to provide the true (absolute) prevalence of your species.
}
\value{
A numeric vector (if 'model' is provided or if 'pred' is also a numeric vector) or a RasterLayer (if 'pred' is a RasterLayer) of the favourability values corresponding to the input probability and input proportion of presences.
}
\references{
Acevedo P. & Real R. (2012) Favourability: concept, distinctive characteristics and potential usefulness. Naturwissenschaften 99: 515-522

Albert C.H. & Thuiller W. (2008) Favourability functions versus probability of presence: advantages and misuses. Ecography 31: 417-422.

Barbosa A.M.E. (2006) Modelacion de relaciones biogeograficas entre predadores, presas y parasitos: implicaciones para la conservacion de mamiferos en la Peninsula Iberica. PhD Thesis, University of Malaga (Spain).

Rangel T.F.L.V.B, Diniz-Filho J.A.F & Bini L.M. (2010) SAM: a comprehensive application for Spatial Analysis in Macroecology. Ecography 33: 46-50.

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{multGLM}}
}
\examples{

# obtain a probability model and its predictions:

data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + 
AltitudeRange + HabitatDiversity + HumanPopulation, 
family = binomial))

prob <- predict(mod, data = rotif.env, type = "response")


# obtain predicted favourability in different ways:

Fav(model = mod)

Fav(obs = rotif.env$Abrigh, pred = prob)

Fav(pred = mod$fitted.values, n1n0 = c(112, 179))

Fav(pred = mod$fitted.values, sample.preval = 0.38)
}
\keyword{ models }
