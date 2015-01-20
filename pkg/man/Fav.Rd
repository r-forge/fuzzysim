\name{Fav}
\alias{Fav}
\title{
Favourability
}
\description{
Environmental (prevalence-independent) favourability for a species' presence
}
\usage{
Fav(obs = NULL, pred = NULL, n1n0 = NULL, model = NULL, sample.preval = NULL, 
method = "RBV", true.preval = NULL)
}
\arguments{
  \item{obs}{
a vector of 1-0 values of a modelled binary variable.
}
  \item{pred}{
a vector of predicted probability values for \code{obs}, given e.g. by logistic regression.
}
  \item{n1n0}{
alternatively to obs, an integer vector of length 2 providing the total number of ones and zeros, in this order. This argument is ignored if the \code{obs} vector is provided.
}
  \item{model}{
alternatively to all of the above, a model object of class \code{"glm"}. If provided, will override any values provided in the arguments described above.
}
  \item{sample.preval}{
alternatively to \code{obs} or \code{n1n0}, the prevalence (proportion of positive cases) of the modelled binary variable in the modelled data.
}
  \item{method}{
either \code{"RBV"} for the original Real, Barbosa & Vargas (2006) procedure, or \code{"AT"}" for the modification proposed by Albert & Thuiller (2008) (but see Acevedo & Real 2012)
  }
  \item{true.preval}{
the true prevalence (as opposed to sample prevalence), necessary if you want to use the AT method.
}
}
\details{
Logistic regression (Generalised Linear Model with binomial error distribution and a logit link) is widely used for modelling species' potential distributions using presence/absence data and a set of categorical or continuous predictor variables. However, this GLM incorporates the prevalence (relative proportion of presences and absences) of the species in the training sample, which affects the probability values produced.

Barbosa (2006) and Real, Barbosa & Vargas (2006) proposed an environmental favourability function which is based on logistic regression but cancels out uneven proportions of presences and absences in the modelled data. Favourability thus assesses the extent to which the environmental conditions change the probability of occurrence of a species with respect to its overall prevalence in the study area. Model predictions can, therefore, be directly compared between species with different prevalences. The favourability function is implemented in the modEvA package and is also in the SAM (Spatial Analysis in Macroecology) software (Rangel et al. 2010).

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
\note{
}
\seealso{
\code{\link{glm}}, \code{\link{multGLM}}, \code{\link{prevalence}}
}
\examples{

# obtain a probability model and its predictions:

data(rotif.env)

names(rotif.env)

mod <- with(rotif.env, glm(Abrigh ~ Area + Altitude + AltitudeRange + 
HabitatDiversity + HumanPopulation, family = binomial))

prob <- predict(mod, data = rotif.env, type = "response")


# obtain predicted favourability in different ways:

Fav(model = mod)

Fav(obs = rotif.env$Abrigh, pred = prob)

Fav(pred = mod$fitted.values, n1n0 = c(112, 179))

Fav(pred = mod$fitted.values, sample.preval = 0.3849)
}
\keyword{ probability }
\keyword{ prevalence }
\keyword{ GLM }
\keyword{ models }
\keyword{ prediction }

