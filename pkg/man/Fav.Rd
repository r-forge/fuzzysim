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
sample.preval = NULL, method = "RBV", true.preval = NULL)
}
\arguments{
  \item{model}{
a model object of class \code{"glm"} and binomial family.
}
  \item{obs}{
a vector of the 1/0 values of the modelled binary variable. This argument is ignored if \code{model} is provided.
}
  \item{pred}{
a vector of predicted probability values for \code{obs}, given e.g. by logistic regression. This argument is ignored if \code{model} is provided.
}
  \item{n1n0}{
alternatively to \code{obs}, an integer vector of length 2 providing the total numbers of modelled ones and zeros, in this order. Ignored if \code{obs} or \code{model} is provided.
}
  \item{sample.preval}{
alternatively to \code{obs} or \code{n1n0}, the prevalence (proportion of positive cases) of the modelled binary variable in the modelled data. Ignored if \code{model} is provided.
}
  \item{method}{
either \code{"RBV"} for the original Real, Barbosa & Vargas (2006) procedure, or \code{"AT"}" for the modification proposed by Albert & Thuiller (2008) (but see Details).
  }
  \item{true.preval}{
the true prevalence (as opposed to sample prevalence), necessary if you want to use the AT method.
}
}
\details{
Logistic regression (Generalised Linear Model with binomial error distribution and a logit link) is widely used for modelling species' potential distributions using presence/absence data and a set of categorical or continuous predictor variables. However, this GLM incorporates the prevalence (proportion of presences) of the species in the training sample, which affects the probability values produced. Barbosa (2006) and Real, Barbosa & Vargas (2006) proposed an environmental favourability function which is based on logistic regression but cancels out uneven proportions of presences and absences in the modelled data. Favourability thus assesses the extent to which the environmental conditions change the probability of occurrence of a species with respect to its overall prevalence in the study area. Model predictions become, therefore, directly comparable among species with different prevalences. The favourability function is implemented in the \pkg{fuzzySim} package and is also in the SAM (Spatial Analysis in Macroecology) software (Rangel et al. 2010).

Using simulated data, Albert & Thuiller (2008) proposed a modification to the favourability function, but it requires knowing the true prevalence of the species (not just the prevalence in the studied sample), which is rarely possible in real-world modelling. Besides, this suggestion was based on the misunderstanding that the favourability function was a way to obtain the probability of occurrence when prevalence differs from 50\%, which is incorrect (see Acevedo & Real 2012).

To get environmental favourability with either the Real, Barbosa & Vargas ("RBV") or the Albert & Thuiller ("AT") method, you just need to get a probabilistic model (e.g. logistic regression) from your data and then use the \code{Fav} function. Input data for this function are either a model object resulting from the \code{\link{glm}} function, or the presences-absences (1-0) of your species and the corresponding presence probability values, obtained e.g. with predict(mymodel, mydata, type = "response"). Alternatively to the presences-absences, you can provide either the sample prevalence or the numbers of presences and absences. In case you want to use the "AT" method, you also need to provide the true (absolute) prevalence of your species.
}
\value{
A numeric vector of the favourability values corresponding to the input probability values.
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
\code{\link{glm}}, \code{\link{multGLM}}
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
