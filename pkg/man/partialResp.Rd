\name{partialResp}
\alias{partialResp}
\title{Partial response plot(s) for probability or favourability}
\description{
This function produces partial response plot(s) for probability or favourability, for one to all variables in a 'glm' model object.
}
\usage{
partialResp(model, vars = NULL, Fav = FALSE, se.mult = 1.96, plot.points = FALSE,
ylim = c(0, 1), reset.par = TRUE, ...)
}
\arguments{
  \item{model}{a model object of class 'glm' and family 'binomial'.}
  \item{vars}{character vector of the name(s) of the variable(s) for which to compute the partial response plot. The default is \code{NULL}, for all variables in 'model'.}
  \item{Fav}{logical value indicating whether to compute the response curve(s) for \code{\link{Fav}}ourability instead of predicted probability. Default \code{FALSE}.}
  \item{se.mult}{numeric value indicating the multiplier for the standard error of the predictions. The default is 1.96, for the 95\% confidence interval. If set to 0, no confidence intervals are plotted.}
  \item{plot.points}{logical value indicating whether to plot the points of predicted probability (or favourability, if \code{Fav=TRUE}) against the values of the plotted variable. Default \code{FALSE}.}
  \item{ylim}{either a numeric vector of length 2 indicating the minimum and maximum value for the y-axis, or character value \code{"auto"} for fitting the axis limits to the existing values in each plot. The default is \code{c(0, 1)}, for all curves to be directly comparable.}
  \item{reset.par}{logical. If TRUE (the default), plotting \code{\link{par}}ameters are changed by the function and reset in the end. FALSE can be useful if the user wants to set their own parameters (like 'mfrow' or 'mar') and combine this with other plots.}
  \item{\dots}{some additional arguments that can be passed to \code{\link{plot}}, e.g. 'main', 'cex.axis' or 'cex.lab'.}
}
\details{
  Each variable is plotted at intervals of 1/100th of its range. Confidence intervals are computed as the value plus/minus the standard error multiplied by 'se.mult' (default 1.96, for the 95\% confidence interval). To avoid the confidence intervals exceeding the 0:1 interval that's possible for probability, the standard error is computed on the predictions at the scale of the predictors (i.e., computed with \code{type="link"}), and then back-transformed with \code{model$family$linkinv} (see \url{https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms} -- thanks to Gavin Simpson for this post!).
}
\value{
  A partial response plot for each variable.
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link[plotmo:plotmo]{plotmo::plotmo}}, \code{\link[predicts:partialResponse]{predicts::partialResponse}}
}
\examples{
data(rotif.env)

form <- reformulate(names(rotif.env)[5:17], "Kcochl")

mod <- glm(form, data = rotif.env, family = binomial)

partialResp(mod)

partialResp(mod, Fav = TRUE)

partialResp(mod, Fav = TRUE, plot.points = TRUE)
}
\keyword{models}
\keyword{graphics}
