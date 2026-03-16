\name{Fav}
\alias{Fav}
\title{
Favourability (probability without the effect of sample prevalence)
}
\description{
Computes prevalence-independent favourability for a species' presence, based on a presence/(pseudo)absence model object, or on a vector of predicted probability values plus either the modelled binary response variable, or the total numbers of modelled ones and zeros, or the prevalence (proportion of ones) in the modelled binary response (i.e., in the model training data). It can also do the inverse operation, i.e. compute presence probability from a vector of favourability values and the modelled prevalence.
}
\usage{
Fav(model = NULL, obs = NULL, pred = NULL, n1n0 = NULL, sample.preval = NULL,
method = "RBV", true.preval = NULL, inv = FALSE, verbosity = 2)
}
\arguments{
  \item{model}{
optional binary-response presence/(pseudo)absence model object of class "glm", "gam", "gbm", "randomForest" or "bart" (the latter computed with keeptrees=TRUE), computed with weights=NULL (i.e., without weighing presences and absences differently).
}
  \item{obs}{
alternatively to 'model', a vector of the 1 and 0 values of the binary response variable (e.g. presence-absence of a species) in the model training data. This argument is ignored if 'model' is provided.
}
  \item{pred}{
alternatively to 'model', a numeric vector, RasterLayer or SpatRaster of predicted presence probability (not suitability) values, produced by a presence/(pseudo)absence modelling method (computed with weights=NULL). This argument is ignored if 'model' is provided.
}
  \item{n1n0}{
alternatively to 'obs' or 'sample.preval', an integer vector of length 2 providing the total numbers of modelled ones and zeros, respectively, of the binary response variable in the model training data (not necessarily the same as in the raw input data, which may e.g. have NAs or duplicates!). Ignored if 'obs' or 'model' is provided.
}
  \item{sample.preval}{
alternatively to 'obs' or 'n1n0', the prevalence (proportion of ones) of the binary response variable in the model training data (not necessarily the same as in the raw input data, which may e.g. have NAs or duplicates!). Ignored if 'model' is provided.
}
  \item{method}{
either "RBV" for the original Real, Barbosa & Vargas (2006) procedure, or "AT" if you want to try out the (misled) modification proposed by Albert & Thuiller (2008) - but see Details!
  }
  \item{true.preval}{
the true prevalence (as opposed to sample prevalence), necessary if you want to try the "AT" method (but see Details!)
}
  \item{inv}{
logical value (default FALSE) indicating whether to do the inverse operation instead, i.e. compute presence probability from favourability. If TRUE, the user must supply not a 'model' object, but rather a 'pred' vector of favourability values, plus either 'obs' or 'n1n0' or 'sample.preval'.
  }
  \item{verbosity}{
numeric value indicating the amount of messages to display; currently meaningful values are 0, 1, and 2 (the default).
  }
}
\details{
Presence-(pseudo)abence modelling methods, such as GLM, GAM, Random Forests, BRT/GBM or BART, are widely used for modelling species' potential distributions. These models predict presence probability, which incorporates the prevalence (proportion of presences) of the species in the modelled sample. It thus reflects the natural fact that widespread species are more likely to occur than rare species, independently of (or under equally good) environmental conditions.

The favourability function (Barbosa, 2006; Real, Barbosa & Vargas, 2006) removes this effect of prevalence from presence probability predictions. Unlike the raw probability, favourability is standardized: a value of 0.5 always means that the local environmental conditions neither increase nor decrease the species' probability of occurrence beyond its general prevalence. It is thus a consistent "neutral" baseline that can be compared and combined across species, regions, spatial resolutions, and time periods.

Using simulated data, Albert & Thuiller (2008) proposed a modification to the favourability function which requires knowing the true prevalence of the species (not just the prevalence in the modelled sample), though this is rarely possible in real-world modelling. Besides, this suggestion was based on the misunderstanding that the favourability function was a way to obtain the probability of occurrence when prevalence differs from 50\%, which is incorrect (see Acevedo & Real 2012).

To get environmental favourability, you can use the 'Fav' function with model predictions of presence probability, together with the prevalence or the presence/absence ratio in the actually modelled sample (which is not necessarily the same as in the raw presence/(pseudo)absence data! e.g. if duplicates are automatically removed, or some presences fall outside the utilized pixels, or pseudoabsences outnumber the available pixels). Input data for 'Fav' are either a model object of an implemented class, or the vector of modelled presences-absences (1-0) of your species and the corresponding presence probability values, obtained e.g. with predict(mymodel, mydata, type = "response"). Alternatively to the presences-absences, you can provide either the sample prevalence or the numbers of presences and absences in the dataset that was modelled to generate the presence probabilities. In case you want to use the "AT" method (but see Acevedo & Real 2012!), you also need to provide the true (besides the sample) prevalence of your species.
}
\value{
If 'model' is provided or if 'pred' is a numeric vector, the function returns a numeric vector of the favourability values. If 'model' is not provided (which would override other arguments) and 'pred' is a RasterLayer or a SpatRaster, the function returns an object of the same class, containing the favourability values.
}
\note{
This function is applicable only to presence probability values obtained without weighting presences and absences differently (i.e. with weights=NULL), thus reflecting the sample prevalence, which is generally the default in presence/absence modelling functions (like \code{\link{glm}}). Note, however, that some modelling packages may use different defaults when calling these functions, e.g. biomod2::BIOMOD_Modeling() with automatically generated pseudo-absences.
}
\references{
Acevedo P. & Real R. (2012) Favourability: concept, distinctive characteristics and potential usefulness. Naturwissenschaften 99: 515-522

Albert C.H. & Thuiller W. (2008) Favourability functions versus probability of presence: advantages and misuses. Ecography 31: 417-422.

Barbosa A.M.E. (2006) Modelacion de relaciones biogeograficas entre predadores, presas y parasitos: implicaciones para la conservacion de mamiferos en la Peninsula Iberica. PhD Thesis, University of Malaga (Spain).

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

Fav(pred = mod$fitted.values, sample.preval = prevalence(model = mod))
}
\keyword{ models }
