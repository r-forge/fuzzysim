\name{fuzzyOverlay}
\alias{fuzzyOverlay}
\title{
Row-wise overlay operations based on fuzzy logic
}
\description{
Logical and set operations are useful for comparative distribution modelling, to assess consensus or mismatches between the predictions of different models, and to quantify differences between models obtained for different time periods. Fuzzy set theory (Zadeh 1965, Barbosa & Real 2012) allows performing such operations without converting the predictions from continuous to binary, with the inherent application of arbitrary thresholds and over-simplification of model predictions. The result is a continuous numerical value quantifying the intersection, union, sum, or other operation among model predictions, whether binary or continuous.
}
\usage{
fuzzyOverlay(data, overlay.cols = 1:ncol(data), op = "intersection", 
na.rm = FALSE, round.digits = 2)
}
\arguments{
  \item{data}{
matrix or data frame containing the model predictions to compare.
}
  \item{overlay.cols}{
vector of the names or index numbers of the columns to compare. The default is all columns in \code{data}.
}
  \item{op}{
character value indicating the operation to perform between the prediction columns in \code{data}. Can be "consensus" for the arithmetic mean of predictions (or the fuzzy equivalent of the proportion of models that agree that the species occurs at each site), "fuzzy_and" or "intersection" for fuzzy intersection; "fuzzy_or" or "union" for fuzzy union; "prob_and" or "prob_or" for probabilistic and/or, respectively (see Details); "maintenance" for the values where all predictions for the same row (rounded to the number of digits specified in the next argument) are the same. If \code{data} has only two columns to compare, you can also calculate"xor" for exclusive 'or', "AnotB"" for the the occurrence of the species in column 1 in detriment of that in column 2, "expansion" for the prediction increase in rows where column 2 has higher values than column 1, "contraction" for the prediction decrease in rows where column 2 has lower values than column 1, or "change" for a mix of the latter two,  with positive values where there has been an increase and negative values where there was decrease in favourability from columns 1 to 2. For expansion, contraction and maintenance, rows where the values do not satisfy the condition (i.e. second column larger, smaller, or roughly equal to the first column) get a value of zero.
}
  \item{na.rm}{
logical value indicating if \code{NA} values should be ignored. The default is \code{FALSE}, so rows with \code{NA} in any of the prediction columns get \code{NA} as a result.
}
  \item{round.digits}{
integer value indicating the number of decimal places to be used if op = "maintenance". The default is 2.
}
}
\details{
If your predictions are probabilities, "prob_and" (probabilistic 'and') gives the probability of all species in \code{data} occurring simultaneously by multiplying all probabilities; and "prob_or" (probabilistic 'or') gives the probability of any of them occurring at each site. These can be quite restrictive, though; probabilistic "and" can give particularly irrealistically small values. 

If you have (or convert your probabilities to) favourability predictions, which can be used directly with fuzzy logic (Real et al. 2006; see \code{\link{Fav}} function), you can use "fuzzy_and" or "intersection" to get the favourability for all species co-occurring at each site, and "fuzzy_or" or "union" to get favourability for any of them to occur at each site (Barbosa & Real 2012).
}
\value{
This function returns a vector, with length equal to the number of rows in \code{data}, containing the row-wise result of the operation performed.
}
\references{
Barbosa A.M. & Real R. (2012) Applying fuzzy logic to comparative distribution modelling: a case study with two sympatric amphibians. The Scientific World Journal, 2012, Article ID 428206

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.

Zadeh, L.A. (1965) Fuzzy sets. Information and Control, 8: 338-353
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{fuzSim}}, \code{\link{modOverlap}} and \code{\link{fuzzyRangeChange}} for overall (not row-wise) comparisons among model predictions.
}
\examples{
data(rotif.env)

names(rotif.env)


# get model predictions for 3 of the species in rotif.env:

mods <- multGLM(rotif.env, sp.cols = 18:20, var.cols = 5:17, id.col = 1,
step = TRUE, FDR = TRUE, trim = TRUE)

preds <- mods$predictions[ , c("Abrigh_F", "Afissa_F", "Apriod_F")]


# calculate intersection and union among those predictions:

preds$intersect <- fuzzyOverlay(preds, op = "intersection")

preds$union <- fuzzyOverlay(preds, op = "union")

head(preds)


# imagine you have a model prediction for species 'Abrigh' in a future time
# (here we will create one by randomly jittering the current predictions)

preds$Abrigh_imag <- jitter(preds[ , "Abrigh_F"], amount = 0.2)
preds$Abrigh_imag[preds$Abrigh_imag < 0] <- 0
preds$Abrigh_imag[preds$Abrigh_imag > 1] <- 1


# you can calculate row-wise prediction changes from Abrigh to Abrigh_imag:

preds$Abrigh_exp <- fuzzyOverlay(preds, overlay.cols = c("Abrigh_F", 
"Abrigh_imag"), op = "expansion")

preds$Abrigh_contr <- fuzzyOverlay(preds, overlay.cols = c("Abrigh_F", 
"Abrigh_imag"), op = "contraction")

preds$Abrigh_chg <- fuzzyOverlay(preds, overlay.cols = c("Abrigh_F", 
"Abrigh_imag"), op = "change")

preds$Abrigh_maint <- fuzzyOverlay(preds, overlay.cols = c("Abrigh_F", 
"Abrigh_imag"), op = "maintenance")

head(preds)
}
