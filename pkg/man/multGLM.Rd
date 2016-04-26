\name{multGLM}
\alias{multGLM}
\title{
GLMs for multiple species with multiple options
}
\description{
This function calculates generalized linear models for a set of (species) presence/absence records in a data frame, with a wide set of options for data partition, variable selection, and output form.
}
\usage{
multGLM(data, sp.cols, var.cols, id.col = NULL, family = "binomial",
test.sample = 0, FDR = FALSE, correction = "BY", corSelect = FALSE, 
cor.thresh = 0.8, step = TRUE, trace = 0, start = "null.model", 
direction = "both", select = "AIC", Y.prediction = FALSE, 
P.prediction = TRUE, Favourability = TRUE, group.preds = TRUE, trim = TRUE, ...)
}
\arguments{
  \item{data}{a data frame in wide format (see \code{\link{splist2presabs}}) containing, in separate columns, your species' binary (0/1) occurrence data and the predictor variables.}
  \item{sp.cols}{index numbers of the columns containing the species data to be modelled.}
  \item{var.cols}{index numbers of the columns containing the predictor variables to be used for modelling.}
  \item{id.col}{(optional) index number of column containing the row identifiers (if defined, it will be included in the output \code{predictions} data frame).}
  \item{family}{argument to be passed to the \code{\link{glm}} function; only 'binomial' is implemented in \code{multGLM} so far.}
  \item{test.sample}{a subset of data to set aside for subsequent model testing. Can be a value between 0 and 1 for a proportion of the data to choose randomly (e.g. 0.2 for 20\%), or an  integer number for a particular number of cases to choose randomly among the records in \code{data}, or a vector of integers for the index numbers of the particular rows to set aside, or "Huberty" for his rule of thumb based on the number of variables (Huberty 1994, Fielding & Bell 1997).}
  \item{FDR}{logical value indicating whether to do a preliminary exclusion of variables based on the false discovery rate (see \code{\link{FDR}}). The default is \code{FALSE}.}
  \item{correction}{argument to pass to the \code{\link{FDR}} function if \code{FDR = TRUE}. The default is \code{"BY"} for dealing with dependencies among statistics (Benjamini & Yekutieli 2001), but see \code{\link{p.adjust}} for more options.}
  \item{corSelect}{logical value indicating whether to do a preliminary exclusion of highly correlated variables (see \code{\link{corSelect}}). The default is \code{FALSE}.}
  \item{cor.thresh}{numerical value indicating the correlation threshold to pass to \code{\link{corSelect}} (used only if \code{corSelect = TRUE}).}
  \item{step}{logical, whether to use the \code{\link{step}} function to perform a stepwise variable selection (based on AIC or BIC).}
  \item{trace}{if positive, information is printed during the running of \code{\link{step}}. Larger values may give more detailed information.}
  \item{start}{character, whether to start with the 'null.model' (so that variable selection starts forward) or with the 'full.model' (so selection starts backward). Used only if \code{step = TRUE}.}
  \item{direction}{argument to be passed to \code{\link{step}} specifying the direction of variable selection ('forward', 'backward' or 'both'). Used only if \code{step = TRUE}.}
  \item{select}{character string specifying the criterion for stepwise selection of variables. Options are "AIC" (Akaike's Information Criterion; Akaike, 1973), the default; or BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC or SBIC; Schwarz, 1978). Used only if \code{step = TRUE}.}
  \item{Y.prediction}{logical, whether to include output predictions in the scale of the predictor variables (\code{type = "link"} in \code{\link{predict.glm}}).}
  \item{P.prediction}{logical, whether to include output predictions in the scale of the response variable, i.e. probability (\code{type = "response"} in \code{\link{predict.glm}}).}
  \item{Favourability}{logical, whether to apply the \code{\link{Fav}}ourability function to extract the effect of prevalence on probability (Real et al. 2006) and include its results in the output.}
  \item{group.preds}{logical, whether to group together predictions of similar type (\code{Y}, \code{P} or \code{F}) in the output \code{predictions} table (e.g. if \code{FALSE}: sp1_Y, sp1_P, sp1_F, sp2_Y, sp2_P, sp2_F; if \code{TRUE}: sp1_Y, , sp2_Y, sp1_P, sp2_P, sp1_F, sp2_F).}
  \item{trim}{logical, whether to trim non-significant variables off the models using the \code{\link{modelTrim}} function; can be used whether or not \code{step} is TRUE; works as a backward variable elimination procedure based on significance.}
  \item{\dots}{additional arguments to be passed to \code{\link{modelTrim}}.}
}
\details{
This function automatically calculates binomial GLMs for one or more species (or other binary variables) in a data frame. The function can optionally perform \code{\link{step}}wise variable selection (and it does so by default) instead of forcing all variables into the models, starting from either the null model (the default, so selection starts forward) or from the full model (so selection starts backward) and using Akaike's information criterion (AIC) as a variable selection criterion. Instead or subsequently, it can also perform stepwise removal of non-significant variables from the models using the \code{\link{modelTrim}} function.

There is also an optional preliminary selection of non-correlated variables, and/or of variables with a significant bivariate relationship with the response, based on the false discovery rate (\code{\link{FDR}}). Note, however, that some variables can be significant in a multivariate model even if they would not have been selected by FDR.

\code{\link{Fav}}ourability is also calculated, removing the effect of species prevalence from occurrence probability and thus allowing direct comparisons between models (Real et al. 2006).

By default, all data are used in model training, but you can define an optional \code{test.sample} to be reserved for model testing afterwards. You may also want to do a previous check for \code{\link{multicol}}linearity among variables, e.g. the variance inflation factor (VIF).

The \code{multGLM} function will create a list of the resulting models (each with the name of the corresponding species column) and a data frame with their predictions (Y, P and/or F, all of which are optional). If you plan on representing these predictions in a GIS based on .dbf tables, remember that dbf only allows up to 10 characters in column names; \code{multGLM} predictions will add 2 characters (_Y, _P and/or _F) to each of your species column names, so use species names/codes with up to 8 characters in the data set that you are modelling. You can create (sub)species name abbreviations with the \code{\link{spCodes}} function.
}
\value{
This function returns a list with the following components:
\item{predictions}{a data frame with the model predictions (if either of \code{Y.prediction}, \code{P.prediction}, or \code{Favourability} are \code{TRUE}).}
\item{models}{a list of the resulting model objects.}
}
\references{
Akaike, H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Benjamini Y. & Yekutieli D. (2001) The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29: 1165-1188.

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Huberty C.J. (1994) Applied Discriminant Analysis. Wiley, New York, 466 pp.
Schaafsma W. & van Vark G.N. (1979) Classification and discrimination problems with applications. Part IIa. Statistica Neerlandica 33: 91-126

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.
}
\author{
A. Marcia Barbosa
}
\note{
}
\seealso{
\code{\link{glm}}, \code{\link{Fav}}, \code{\link{step}}, \code{\link{modelTrim}}, \code{\link{multicol}}, \code{\link{corSelect}}
}
\examples{
data(rotif.env)

names(rotif.env)


# make models for 3 of the species in rotif.env:

mods <- multGLM(rotif.env, sp.cols = 45:47, var.cols = 5:17, id.col = 1,
step = TRUE, FDR = TRUE, trim = TRUE)

names(mods)

head(mods$predictions)

names(mods$models)

mods$models[[1]]

mods$models[["Ttetra"]]

}
\keyword{ models }
\keyword{ regression }
\keyword{ multivariate }
