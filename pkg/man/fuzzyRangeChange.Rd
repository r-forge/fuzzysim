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
na.rm = TRUE, round.digits = 2,
measures = c("Gain", "Loss", "Stable positive", "Stable negative", "Balance"),
plot = TRUE, plot.type = "lollipop", x.lab = TRUE, ...)
}
\arguments{
  \item{pred1}{numeric vector or SpatRaster layer containing the predictions (between 0 and 1) of the model that will serve as reference.}
  \item{pred2}{numeric vector or SpatRaster layer containing the predictions (between 0 and 1) of the model whose change will be calculated. Must be of the same dimensions and in the same order as 'pred1'.}
  \item{number}{logical value indicating if results should include the fuzzy number of cases. The default is TRUE.}
  \item{prop}{logical value indicating if results should include the proportion of the total number of cases. The default is TRUE.}
  \item{na.rm}{logical value indicating whether NA values should be ignored. The default is TRUE.}
  \item{round.digits}{argument to pass to \code{\link{fuzzyOverlay}}, indicating the number of decimal places to which to round 'pred' for calculating 'maintenance' or 'stability'. The default is 2.}
  \item{measures}{character vector listing the range change measures to calculate. The default includes all available measures.}
  \item{plot}{logical value indicating whether to plot the results. The default is TRUE.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "lollipop" (the default) or "barplot".}
  \item{x.lab}{logical value indicating whether to add the x axis labels to the plot (i.e., the names below each lollipop or bar). The default is TRUE, but users may set it to FALSE and then add labels differently (e.g. with different names or rotations).}
  \item{\dots}{additional arguments to pass to \code{barplot} or to \code{modEvA::lollipop} (if plot=TRUE).}
}
\value{
This function returns a data frame with the following values in different rows (among those included in 'measures'):
\item{Gain}{sum of the predicted values that have increased from 'pred1' to 'pred2' (fuzzy equivalent of the number of localities that gained presence)}
\item{Loss}{sum of the predicted values that have decreased from 'pred1' to 'pred2' (fuzzy equivalent of the number of localities that lost presence)}
\item{Stable positive}{fuzzy equivalent of the number of (predicted) presences that have remained as such (when rounded to 'round.digits') between 'pred1' and 'pred2'}
\item{Stable negative}{fuzzy equivalent of the number of (predicted) absences that have remained as such (when rounded to 'round.digits') between 'pred1' and 'pred2')}
\item{Balance}{sum of the change in predicted values from 'pred1' to 'pred2' (fuzzy equivalent of the balance of gained and lost presences)}
If number=TRUE (the default), there is a column named "Number" with the number of localities in each of the above categories. If prop=TRUE (the default), there is a column named "Proportion" in which this number is divided by the total number of reference values (i.e., the fuzzy range or fuzzy non-range size). If plot=TRUE (the default), a plot is also produced representing the last column of the result data frame.
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{fuzSim}}, \code{\link{modOverlap}} for other ways to compare models; \code{\link{fuzzyOverlay}} for row-wise or pixel-wise model comparisons
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

fuzzyRangeChange(fav_current, fav_imag, las = 2)

fuzzyRangeChange(fav_current, fav_imag, prop = FALSE)

fuzzyRangeChange(fav_current, fav_imag, ylim = c(-0.3, 0.3))

fuzzyRangeChange(fav_current, fav_imag, plot.type = "barplot")
}
