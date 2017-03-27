\name{fuzzyRangeChange}
\alias{fuzzyRangeChange}
\title{
Range change based on continuous (fuzzy) values
}
\description{
This function quantifies overall range change (expansion, contraction, maintenance and balance) based on either presence-absence data or the continuous predictions of two models.
}
\usage{
fuzzyRangeChange(pred1, pred2, number = TRUE, prop = TRUE, 
na.rm = TRUE, round.digits = 2, measures = c("Gain", "Loss", 
"Stable presence", "Stable absence", "Balance"), plot = TRUE, ...)
}
\arguments{
  \item{pred1}{
numeric vector containing the predictions (between 0 and 1) of the model that will serve as reference.
}
  \item{pred2}{
numeric vector containing the predictions (between 0 and 1) of the model whose change will be calculated. Must be of the same length and in the same order as \code{pred1}.
}
  \item{number}{
logical value indicating if results should include the fuzzy number of cases. The default is TRUE.
}
  \item{prop}{
logical value indicating if results should include the proportion of the total number of cases. The default is TRUE.
}
  \item{na.rm}{
logical value indicating whether NA values should be ignored. The default is TRUE.
}
  \item{round.digits}{
argument to pass to \code{\link{fuzzyOverlay}}, indicating the number of decimal places to which to round \code{pred} for calculating 'maintenance' or 'stability'. The default is 2.
}
  \item{measures}{
character vector listing the range change measures to calculate. The default is all available measures.
}
  \item{plot}{
logical value indicating whether to make a barplot with the results. The default is TRUE.
}
  \item{\dots}{additional arguments to be passed to the \code{barplot} function (if \code{plot = TRUE}).}
}
\value{
This function returns a data frame with the following values in different rows (among those that are included in \code{measures}):
\item{Gain}{sum of the predicted values that have increased from \code{pred1} to \code{pred2} (fuzzy equivalent of the number of gained presences)}
\item{Loss}{sum of the predicted values that have decreased from \code{pred1} to \code{pred2} (fuzzy equivalent of the number of lost presences)}
\item{Stable_presence}{fuzzy equivalent of the number of predicted presences that have remained as such (when rounded to \code{round.digits}) between \code{pred1} and \code{pred2}}
\item{Stable_absence}{fuzzy equivalent of the number of predicted absences that have remained as such (when rounded to \code{round.digits}) between \code{pred1} and \code{pred2})}
\item{Balance}{sum of the change in predicted values from \code{pred1} to \code{pred2} (fuzzy equivalent of the balance of gained and lost presences)}
If \code{prop = TRUE} (the default), there is an additional column named "Proportion" in which these values are divided by the total number of reference values (i.e., the fuzzy range or non-range size). If \code{plot = TRUE} (the default), a barplot is also produced representing the last column of the result data frame.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{fuzSim}}, \code{\link{modOverlap}} for other ways to compare models; \code{\link{fuzzyOverlay}} for row-wise model comparisons
}
\examples{
# get an environmental favourability model for a rotifer species:

data(rotif.env)

names(rotif.env)

fav_current <- multGLM(rotif.env, sp.cols = 18, var.cols = 5:17, 
step = TRUE, FDR = TRUE, trim = TRUE, P = FALSE, Fav = TRUE) $
predictions


# imagine you have a model prediction for this species in a future time
# (here we will create one by randomly jittering the current predictions)

fav_imag <- jitter(fav_current, amount = 0.2)
fav_imag[fav_imag < 0] <- 0
fav_imag[fav_imag > 1] <- 1


# calculate range change given by current and imaginary future predictions:

fuzzyRangeChange(fav_current, fav_imag)

fuzzyRangeChange(fav_current, fav_imag, number = FALSE)

fuzzyRangeChange(fav_current, fav_imag, ylim = c(-1, 1), 
ylab = "Proportional change")
}
