\name{getPreds}
\alias{getPreds}
\title{
Get model predictions
}
\description{
This function allows getting the predictions of multiple models when applied to a given dataset. It can be useful if you have a list of model objects (e.g. resulting from \code{\link{multGLM}}) and want to apply them to a new data set containing the same variables for another region or time period. There are options to include the logit link (\code{Y}) and/or \code{Favourability} (see \code{\link{Fav}}).
}
\usage{
getPreds(data, models, id.col = NULL, Y = FALSE, P = TRUE, 
Favourability = TRUE, incl.input = FALSE)
}
\arguments{
  \item{data}{
an object of class either 'data.frame' or 'RasterStack' to which to apply the 'models' (below) to get their predictions; must contain all variables (with the same names, case-sensitive) included in any of the 'models'.
}
  \item{models}{
an object of class 'list' containing one or more model objects, obtained e.g. with function \code{\link{glm}} or \code{\link{multGLM}}.
}
  \item{id.col}{
optionally, the index number of a column of 'data' containing row identifiers, to be included in the result. Ignored if \code{incl.input = TRUE}, or if 'data' is a RasterStack rather than a data frame.
}
  \item{Y}{
logical, whether to include the logit link (y) value in the predictions.
}
  \item{P}{
logical, whether to include the probability value in the predictions.
}
  \item{Favourability}{
logical, whether to include Favourability in the predictions (see \code{\link{Fav}}).
}
  \item{incl.input}{
logical, whether to include input columns in the output data frame (if the 'data' input is a data frame as well). The default is FALSE.
}
}
\value{
This function returns the model predictions in an object of the same class as the input 'data', i.e. either a data frame or a RasterStack.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{multGLM}}, \code{\link{predict}}
}
\examples{
data(rotif.env)

names(rotif.env)


# identify rotifer data in the Eastern and Western hemispheres:

unique(rotif.env$CONTINENT)

rotif.env$HEMISPHERE <- "Eastern"

rotif.env$HEMISPHERE[rotif.env$CONTINENT \%in\%
c("NORTHERN_AMERICA", "SOUTHERN_AMERICA")] <- "Western"

head(rotif.env)


# separate the rotifer data into hemispheres

east.hem <- rotif.env[rotif.env$HEMISPHERE == "Eastern", ]
west.hem <- rotif.env[rotif.env$HEMISPHERE == "Western", ]


# make models for 3 of the species in rotif.env based on their distribution
# in the Eastern hemisphere:

mods <- multGLM(east.hem, sp.cols = 18:20, var.cols = 5:17, 
id.col = 1, step = FALSE, FDR = FALSE, trim = FALSE)


# get the models' predictions for the Western hemisphere dataset:

preds <- getPreds(west.hem, models = mods$models, P = TRUE, 
Favourability = TRUE)

head(preds)

}
\keyword{ model }
\keyword{ prediction }
