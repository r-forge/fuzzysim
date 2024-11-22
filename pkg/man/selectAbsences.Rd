\name{selectAbsences}
\alias{selectAbsences}
\title{
Select (spatially biased) absence rows.
}
\description{
This function takes a matrix or data frame containing species presence (1) and absence (0) data, and it selects among the absence rows to stay within a given number or ratio of absences, and/or within and/or beyond a given distance to the presences. Optionally, absences can be selected with higher probability towards the vicinity of presences, to reproduce survey bias; or according to a user-provided bias raster.
}
\usage{
selectAbsences(data, sp.cols, coord.cols = NULL, CRS = NULL, min.dist = NULL,
max.dist = NULL, n = NULL, mult.p = NULL, bias = FALSE, bunch = FALSE,
dist.mat = NULL, seed = NULL, plot = !is.null(coord.cols), df = TRUE,
verbosity = 2)
}
\arguments{
  \item{data}{a data frame or an object that can be coerced to such (e.g. a matrix, tibble or SpatVector) containing a column with the species' presence (1) and absence (0) records, with localities in rows; and (if distance or spatial bias are required) two columns with the spatial coordinates, x and y.}
  \item{sp.cols}{names or index numbers of the columns containing the species presences (1) and absences (0) in 'data'.}
  \item{coord.cols}{names or index numbers of the columns containing the spatial coordinates in 'data' (x and y, or longitude and latitude, in this order). Needed if distance or spatial bias are required.}
\item{CRS}{coordinate reference system of the 'coord.cols' in 'data', in one of the following formats: WKT/WKT2, <authority>:<code>, or PROJ-string notation (see \code{\link[terra:crs]{terra::crs()}}). Ignored if 'dist.mat' is provided. Otherwise, if 'CRS' is provided and the 'terra' package is installed, distances are computed with \code{\link[terra:distance]{terra::distance()}}, thus accounting for the curvature of the Earth.}
  \item{min.dist}{(optional) numeric value specifying the minimum distance (in the same units as 'coord.cols') at which selected absences should be from the presences.}
  \item{max.dist}{(optional) numeric value specifying the maximum distance (in the same units as 'coord.cols') at which selected absences should be from the presences.}
  \item{n}{(optional) integer value specifying the number of absence rows to select. Can also be specified as a ratio -- see 'mult.p' below.}
  \item{mult.p}{(optional) numeric value specifying how many times the number of presences to use as 'n' (e.g. 10 times as many absences as presences). Ignored if 'n' is not NULL.}
  \item{bias}{either a logical value TRUE to make the selection of absences biased towards the vicinity of presences (which requires specifying 'coord.cols'; may take time and memory for large datasets if 'dist.mat' is not provided); or a SpatRaster layer (quantifying e.g. survey effort or accessibility) with larger pixel values where the selection of absences should be more likely. Note that this layer should have the same approximate spatial resolution as 'data'. The default is FALSE, for no bias.}
  \item{dist.mat}{optional (but recommendable) argument to pass to \code{\link{distPres}}.}
  \item{bunch}{[PENDING IMPLEMENTATION] logical value specifying if the selected absences should concentrate around presences in proportion to their local density, as in Vollering et al. (2019). The default is FALSE.}
  \item{seed}{(optional) integer value to pass to \code{\link{set.seed}} specifying the random seed to use for sampling among the absences.}
  \item{plot}{logical value specifying whether to plot the result. The default is TRUE if 'coord.cols' are provided.}
  \item{df}{logical value specifying whether to return a dataframe with the input 'data' after removal of the non-selected absences. The default is TRUE. If set to FALSE, the output is a logical vector specifying if each row of 'data' was selected or not.}
  \item{verbosity}{numeric value indicating the amount of messages to display. Choose 0 for no messages.}
}
\details{
Species occurrence data typically incorporate two probability distributions: the actual probability of the species being present, and the probability of it being recorded if it was present (Merow et al. 2013). Thus, any covariation between recording probability and the predictor variables can bias the predictions of species distribution models (Yackulic et al. 2013).

Methods to correct for this bias include the selection of (pseudo)absences preferably towards the vicinity of presence records, in order to reproduce the survey bias. This function implements this strategy in several (alternative or complementary) ways: 1) selecting absences within and/or beyond a given distance from presences; 2) biasing the random selection of absences, making it more likely towards the vicinity of presences (providing the 'prob' argument in \code{\link{sample}} with the result of \code{\link{distPres}}); or [PENDING IMPLEMENTATION!] 3) bunching up the absences preferably around the areas with higher densities of presences (Vollering et al. 2019).

More recent versions allow using a raster map of weights (bias layer), with higher pixel values where absence selection should be proportionaly more likely, and zero or NA where pseudoabsence points should not be placed. This layer should normally reflect survey effort or a proxy, such as accessibility -- e.g. proximity to roads or populated areas, human footprint (see e.g. geodata::footprint) or travel time (e.g. geodata::travel_time). Users should provide  the bias layer at aproximately the same spatial resolution as the 'coord.cols' in the input 'data'.
}
\value{
This function returns the 'data' input after removal of the non-selected absences, or (if df=FALSE) a logical vector specifying if each row of 'data' was selected or not. If plot=TRUE and provided 'coord.cols', it also plots the presences (blue "plus" signs), the selected absences (red "minus" signs) and the excluded absences (orange dots).
}
\references{
Vollering J., Halvorsen R., Auestad I. & Rydgren K. (2019) Bunching up the background betters bias in species distribution models. Ecography, 42: 1717-1727
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{gridRecords}}, \code{\link{sample}}
}
\examples{
data(rotif.env)

head(rotif.env)

names(rotif.env)

table(rotif.env$Burceo)

# select among the absences using different criteria:

burceo_select <- selectAbsences(data = rotif.env, sp.cols = "Burceo",
coord.cols = c("Longitude", "Latitude"), n = 150, seed = 123)

burceo_select <- selectAbsences(data = rotif.env, sp.cols = "Burceo",
coord.cols = c("Longitude", "Latitude"), mult.p = 1.5, seed = 123)

burceo_select <- selectAbsences(data = rotif.env, sp.cols = "Burceo",
coord.cols = c("Longitude", "Latitude"), max.dist = 18)

burceo_select <- selectAbsences(data = rotif.env, sp.cols = "Burceo",
coord.cols = c("Longitude", "Latitude"), max.dist = 18, min.dist = 5,
n = sum(rotif.env$Burceo), bias = TRUE)


\dontrun{
# you can run the next example if you have the 'geodata' package installed:

bias_layer <- geodata::footprint(year = 2009, path = tempfile())

burceo_select <- fuzzySim::selectAbsences(data = rotif.env, sp.cols = "Burceo",
coord.cols = c("Longitude", "Latitude"), n = sum(rotif.env$Burceo),
bias = bias_layer)
}
}
