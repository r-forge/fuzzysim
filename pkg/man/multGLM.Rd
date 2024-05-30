\name{multGLM}
\alias{multGLM}
\title{
GLMs with variable selection for multiple species
}
\description{
This function performs selection of variables and calculates generalized linear models for a set of presence/absence records in a data frame, with a range of options for data partition, variable selection, and output form.
}
\usage{
multGLM(data, sp.cols, var.cols, id.col = NULL, block.cols = NULL,
family = "binomial", test.sample = 0, FDR = FALSE, test = "Chisq",
correction = "fdr", FDR.first = TRUE, corSelect = FALSE, coeff = TRUE,
cor.thresh = ifelse(isTRUE(coeff), 0.8, 0.05), cor.method = "pearson",
step = TRUE, trace = 0, start = "null.model", direction = "both",
select = "AIC", trim = TRUE, Y.prediction = FALSE, P.prediction = TRUE,
Favourability = TRUE, group.preds = TRUE, TSA = FALSE, coord.cols = NULL,
degree = 3, verbosity = 2, test.in = "Rao", test.out = "LRT", p.in = 0.05,
p.out = 0.1, ...)
}
\arguments{
  \item{data}{a data frame in wide format (see \code{\link{splist2presabs}}) containing, in separate columns, your species' binary (0/1) occurrence data and the predictor variables.}
  \item{sp.cols}{names or index numbers of the columns containing the species data to be modelled.}
  \item{var.cols}{names or index numbers of the columns containing the predictor variables to be used for modelling.}
  \item{id.col}{(optional) name or index number of column containing the row identifiers (if defined, it will be included in the output 'predictions' data frame).}
  \item{block.cols}{[UNDER IMPLEMENTATION] names or index numbers of the columns containing predictor variables to force into the model, even when a selection method is applied to the remaining variables.}
  \item{family}{argument to be passed to the \code{\link{glm}} function; currently, only 'binomial' is implemented here.}
  \item{test.sample}{a subset of data to set aside for subsequent model testing. Can be a value between 0 and 1 for a proportion of the data to choose randomly (e.g. 0.2 for 20\%); or an  integer number for a particular number of cases to choose randomly among the records in 'data'; or a vector of integers for the index numbers of the particular rows to set aside; or "Huberty" for his rule of thumb based on the number of variables (Huberty 1994, Fielding & Bell 1997).}
  \item{FDR}{logical value indicating whether to do a preliminary exclusion of variables based on the false discovery rate (see \code{\link{FDR}}). The default is FALSE.}
  \item{test}{argument to pass to the \code{\link{FDR}} function (which, in turn, passes it to \code{\link{anova}}) if FDR=TRUE. The default is currently "Chisq" for back-compatibility.}
  \item{correction}{argument to pass to the \code{\link{FDR}} function if FDR=TRUE. The default is "fdr", but see \code{\link{p.adjust}} for other options.}
  \item{FDR.first}{logical value indicating whether FDR exclusion (if FDR=TRUE) should be applied at the beginning. The default is TRUE. If set to FALSE (and if FDR=TRUE), FDR exclusion will be applied after 'corSelect' below.}
  \item{corSelect}{logical value indicating whether to select among highly correlated variables using \code{\link{corSelect}}. The default is FALSE.}
  \item{coeff}{logical value to pass to \code{\link{corSelect}} (if corSelect=TRUE) indicating whether two variables should be considered highly correlated based on the magnitude of their coefficient (rather than p-value) of correlation. The default is TRUE.}
  \item{cor.thresh}{numerical value indicating the correlation threshold to pass to \code{\link{corSelect}} (if corSelect=TRUE).}
  \item{cor.method}{character value to pass to \code{\link{corSelect}} (if corSelect=TRUE) specifying the correlation coefficient to use. Can be "pearson" (the default), "kendall" or "spearman".}
  \item{step}{logical, whether to perform a stepwise selection of variables, using either the \code{\link{step}} function (if select = "AIC" or "BIC") or the \code{\link{stepwise}} function (if select = "p.value").}
  \item{trace}{if positive, information is printed during the stepwise selection (if step=TRUE). Larger values may give more detailed information.}
  \item{start}{character string specifying whether to start with the 'null.model' (so that variable selection starts forward) or with the 'full.model' (so selection starts backward). Used only if step=TRUE.}
  \item{direction}{if step=TRUE, argument to be passed to \code{\link{step}} or to \code{\link{stepwise}} specifying the direction of variable selection. Can be 'forward', 'backward', or 'both' (the default).}
  \item{select}{character string specifying the criterion for stepwise selection of variables if step=TRUE. Options are the default "AIC" (Akaike's Information Criterion; Akaike, 1973); BIC (Bayesian Information Criterion, also known as Schwarz criterion, SBC or SBIC; Schwarz, 1978); or "p.value" (Murtaugh, 2014). The first two options imply using \code{\link{step}} as the variable selection function, while the last option calls the \code{\link{stepwise}} function. If you set \code{select="p.value"}, we recommend also setting \code{trim=FALSE} to avoid mixing different significance criteria.}
  \item{trim}{logical value indicating whether to trim off non-significant variables from the models using \code{\link{modelTrim}}. This argument is TRUE by default (for back-compatibility), and it can be used whether or not step=TRUE -- e.g. Crawley (2005, p. 208) and Crawley (2007, p. 442 and 601) recommend that \code{\link{step}} (with AIC selection) be followed by significance-based backward elimination).}
  \item{Y.prediction}{logical value indicating whether to include output predictions in the scale of the predictor variables (type = "link" in \code{\link{predict.glm}}).}
  \item{P.prediction}{logical, whether to include output predictions in the scale of the response variable, i.e. probability (type = "response" in \code{\link{predict.glm}}).}
  \item{Favourability}{logical, whether to apply the \code{\link{Fav}}ourability function to remove the effect of prevalence on predicted probability (Real et al. 2006) and include its results in the output.}
  \item{group.preds}{logical, whether to group together predictions of similar type ('Y', 'P' or 'F') in the output 'predictions' table (e.g. if FALSE: sp1_Y, sp1_P, sp1_F, sp2_Y, sp2_P, sp2_F; if TRUE: sp1_Y, , sp2_Y, sp1_P, sp2_P, sp1_F, sp2_F).}
  \item{TSA}{logical, whether to add a trend surface analysis (calculated individually for each species) as a spatial variable in each model (with type="Y" -- see \code{\link{multTSA}} for more details). The default is FALSE. If TRUE, this spatial trend will be treated as any other variable, i.e. also considered by arguments 'FDR', 'corSelect', etc.}
  \item{coord.cols}{argument to pass to \code{\link{multTSA}} (if TSA=TRUE).}
  \item{degree}{argument to pass to \code{\link{multTSA}} (if TSA=TRUE).}
  \item{verbosity}{numeric value indicating the amount of messages to display, from less to more verbose; currently meaningful values are 0, 1, and 2 (the default).}
  \item{test.in}{argument to pass to \code{\link{stepwise}} if select="p.value".}
  \item{test.out}{argument to pass to \code{\link{stepwise}} if select="p.value".}
  \item{p.in}{argument to pass to \code{\link{stepwise}} if select="p.value".}
  \item{p.out}{argument to pass to \code{\link{stepwise}} if select="p.value".}
  \item{\dots}{(for back-compatibility) additional arguments to be passed to \code{\link{modelTrim}} (if trim=TRUE).}
}
\details{
This function automatically calculates binomial GLMs for one or more species (or other binary variables) in a data frame. The function can optionally perform stepwise variable selection using either \code{\link{stepwise}} or \code{\link{step}} (and it does so by default) instead of forcing all variables into the models, starting from either the null model (the default, so selection starts forward) or from the full model (so selection starts backward), and using AIC, BIC or statistical significance as a variable selection criterion. Instead or subsequently, it can also perform stepwise removal of non-significant variables from the models using the \code{\link{modelTrim}} function.

There is also an optional preliminary selection among highly correlated variables, and/or preliminary selection of variables with a significant bivariate relationship with the response, based on the false discovery rate (\code{\link{FDR}}). Note, however, that some variables can be significant in a multivariate model even if they would not have been selected by FDR.

\code{\link{Fav}}ourability can also be calculated by default, removing the effect of training prevalence from occurrence probability and thus allowing direct comparisons between different models (Real et al. 2006; Acevedo & Real 2012).

By default, all data are used in model training, but you can define an optional 'test.sample' to be reserved for model testing afterwards. You may also want to do a previous check for multicollinearity among variables, e.g. the variance inflation factor (VIF), using \code{\link{multicol}}.

The 'multGLM' function will create a list of the resulting models (each with the name of the corresponding species column) and a data frame with their predictions ('Y', 'P' and/or 'F', all of which are optional). If you plan on representing these predictions in a GIS format based on .dbf tables (e.g. ESRI Shapefile), remember that .dbf only allows up to 10 characters in column names; 'multGLM' predictions will add 2 characters (_Y, _P and/or _F) to each of your species column names, so better use species names/codes with up to 8 characters in the data set that you are modelling. You can create (sub)species name abbreviations with the \code{\link{spCodes}} function.
}
\value{
This function returns a list with the following components:
\item{predictions}{a data frame with the model predictions (if either of Y.prediction, P.prediction or Favourability are TRUE).}
\item{models}{a list of the resulting model objects.}
\item{variables}{a list of character vectors naming the variables finally included in each model according to the specified selection criteria.}
}
\note{
With step=TRUE (the default), an error may occur if there are missing values in some of the variables that are selected (see "Warning" in \code{\link{step}}). If this happens, you can use something like data=na.omit(data[ , c(sp.col, var.cols)]).

Thanks are due to Prof. Jose Carlos Guerrero at the University of the Republic (Uruguay), who funded the implementation of the options \code{select="p.value"} and \code{FDR.first=FALSE}.
}
\references{
Acevedo P. & Real R. (2012) Favourability: concept, distinctive characteristics and potential usefulness. Naturwissenschaften, 99:515-522

Akaike, H. (1973) Information theory and an extension of the maximum likelihood principle. In: Petrov B.N. & Csaki F., 2nd International Symposium on Information Theory, Tsahkadsor, Armenia, USSR, September 2-8, 1971, Budapest: Akademiai Kiado, p. 267-281.

Crawley, M.J. (2005) Statistics: An introdution using R. John Wiley & Sons, Ltd.

Crawley, M.J. (2007) The R Book. John Wiley & Sons, Ltd.

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Huberty C.J. (1994) Applied Discriminant Analysis. Wiley, New York, 466 pp.
Schaafsma W. & van Vark G.N. (1979) Classification and discrimination problems with applications. Part IIa. Statistica Neerlandica 33: 91-126

Murtaugh P.A. (2014) In defense of P values. Ecology, 95:611-617

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.

Schwarz, G.E. (1978) Estimating the dimension of a model. Annals of Statistics, 6 (2): 461-464.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{glm}}, \code{\link{step}}, \code{\link{stepwise}}
}
\examples{
data(rotif.env)

names(rotif.env)


# make models for 2 of the species in rotif.env:

mods <- multGLM(rotif.env, sp.cols = 46:47, var.cols = 5:17, id.col = 1,
step = TRUE, FDR = TRUE, trim = TRUE)

names(mods)
head(mods$predictions)
names(mods$models)
mods$models[[1]]
mods$models[["Ttetra"]]


# include each species' spatial trend in the models:

mods <- multGLM(rotif.env, sp.cols = 46:47, var.cols = 5:17, id.col = 1,
step = TRUE, FDR = TRUE, trim = TRUE, TSA = TRUE, coord.cols = c(11, 10))

mods$models[[1]]
mods$models[["Ttetra"]]

mods$variables
# you can then use these selected variables elsewhere
}
\keyword{ models }
\keyword{ regression }
\keyword{ multivariate }
