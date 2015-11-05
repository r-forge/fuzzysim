\name{fuzzyRangeChange}
\alias{fuzzyRangeChange}
\title{
Range change based on continuous (fuzzy) values
}
\description{
This function quantifies overall range change based on the continuous predictions of two species distribution models.
}
\usage{
fuzzyRangeChange(pred1, pred2, na.rm = FALSE, round.digits = 2)
}
\arguments{
  \item{pred1}{
numeric vector containing the predictions (between 0 and 1) of the model that will serve as reference.
}
  \item{pred2}{
numeric vector containing the predictions (between 0 and 1) of the model whose change will be calculated. Must be of the same length and in the same order as \code{pred1}.
}
  \item{na.rm}{
logical value indicating whether NA values should be ignored.
}
  \item{round.digits}{
argument to pass to \code{\link{fuzzyOverlay}}, indicating the number of decimal places to which to round \code{pred} for calculating 'maintenance' or 'stability'. The default is 2.
}
}
\details{
}
\value{
This function returns a list of the following numeric values:
\item{gain}{sum of the predicted values that have increased from \code{pred1} to \code{pred2}}
\item{prop_gain}{\code{gain} divided by the number of elements in \code{pred1}}
\item{loss}{sum of the predicted values that have decreased from \code{pred1} to \code{pred2}}
\item{prop_loss}{\code{loss} divided by the number of elements in \code{pred1}}
\item{stable}{number of predicted values that have remained the same (when rounded to \code{round.digits}) between \code{pred1} and \code{pred2}}
\item{prop_stable}{\code{stable} divided by the number of elements in \code{pred1}}
\item{total_change}{sum of the change in predicted values from \code{pred1} to \code{pred2}}
\item{prop_change}{\code{total_change} divided by the number of elements in \code{pred1}}
}
\references{
}
\author{A. Marcia Barbosa}
\note{
}
\seealso{
\code{\link{fuzSim}}, \code{\link{modOverlap}} for other ways to compare models; \code{\link{fuzzyOverlay}} for row-wise model comparisons
}
\examples{
# get an environmental favourability model for a rotifer species:

data(rotif.env)

names(rotif.env)

fav_current <- multGLM(rotif.env, sp.cols = 18, var.cols = 5:17, step = TRUE, 
FDR = TRUE, trim = TRUE, P = FALSE, Fav = TRUE) $ predictions


# imagine you have a model prediction for this species in a future time
# (here we will create one by randomly jittering the current predictions)

fav_imag <- jitter(fav_current, amount = 0.2)


# calculate range change given by current and imaginary future predictions:

fuzzyRangeChange(fav_current, fav_imag)

}
\keyword{  }
