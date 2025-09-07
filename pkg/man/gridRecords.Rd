\name{gridRecords}
\alias{gridRecords}
\title{
Grid (or thin) point occurrence records to the resolution of a raster map
}
\description{
This function takes a (single or multi-layer) SpatRaster (or a Raster* object for backward compatibility, though this is no longer being optimized) and a set of spatial coordinates of a species' presence (and optionally absence) records, and it returns a data frame of the presences and absences with their raster values in the grid of pixels (cells). This is analogous to removing duplicates and thinning points (both presences and absences) with a distance equal to the raster pixel size.
}
\usage{
gridRecords(rst, pres.coords, abs.coords = NULL, absences = TRUE,
species = NULL, na.rm = TRUE, plot = FALSE)
}
\arguments{
  \item{rst}{
a Raster* or preferably a SpatRaster object (the latter is processed faster) with the desired spatial resolution and extent for the species presence-(pseudo)absence data, and the layer(s) whose values to extract for those data.
}
  \item{pres.coords}{
a SpatVector of points, or an object inheriting class 'data.frame' with 2 columns containing, respectively, the x and y, or longitude and latitude coordinates (in this order, and in the same coordinate reference system as 'rst'!) of the points where species presence was recorded.
}
  \item{abs.coords}{
(optional) same as 'pres.coords' but for points where the species was not recorded. If abs.coords=NULL and absences=TRUE (the default), all pixels that are not intersected by 'pres.coords' will be returned as absences (of records).
}
  \item{absences}{
logical value indicating whether pixels without presence records should be returned as absences (of records). The default is TRUE.
}
  \item{species}{
(optional) character vector, of the same length as 'nrow(pres.coords)', indicating the species to which each pair of coordinates corresponds. Useful for gridding records of more than one species at a time. Its unique values will be used as column names in the output. If this argument is specified, 'abs.coords' cannot be used and 'plot' is set to FALSE.
}
  \item{na.rm}{
logical value indicating whether pixels with NA in all of the 'rst' layers should be excluded from the output data frame. The default is TRUE.
}
  \item{plot}{
  logical value specifying whether to plot the resulting presences and absences. The default is FALSE (for backward compatibility). Automatically set to FALSE if 'species' is not NULL.
}
}
\details{
See e.g. Baez et al. (2020), where this function was first used to get unique presences and absences from point occurrence data at the spatial resolution of marine raster variables.

You should consider cleaning the coordinates beforehand, e.g. with \code{\link{cleanCoords}}.

If your output has an overly large and/or spatially biased set of absences, it may be recommendable to use \code{\link{selectAbsences}} afterwards.
}
\value{
This function returns a data frame with the following columns:
\item{'presence'}{integer, 1 for the cells (pixels) with one or more presence points; and (if absences=TRUE) 0 for the cells with no presence points, or (if 'abs.coords' are provided) for the cells with one or more absence points AND no presence points. If the 'species' argument is provided, instead of 'presence' you get one column named as each species.}
\item{'x', 'y'}{centroid coordinates of each cell (pixel).}
\item{'cell'}{the pixel identifier in 'rst'.}
\item{one column for each layer in 'rst'}{value of each pixel for each layer.}
If plot=TRUE, the fuction also plots the resulting presences (blue "plus" signs) and absences (red "minus" signs).
}
\note{
This function requires either the \pkg{raster} or the \pkg{terra} package, depending on the class of 'rst'. It may crash for large rasters, in which case it is advisable to first divide them into tiles.
}
\references{
Baez J.C., Barbosa A.M., Pascual P., Ramos M.L. & Abascal F. (2020) Ensemble modelling of the potential distribution of the whale shark in the Atlantic Ocean. Ecology and Evolution, 10: 175-184
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{cleanCoords}}, \code{\link{selectAbsences}}
}
\examples{
\dontrun{

# you can run these examples if you have the 'terra' package installed
require(terra)

# import a raster map and aggregate it to a coarser resolution:

r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
r <- terra::aggregate(r, 6)
plot(r)


# generate some random presence and absence points:

set.seed(123)
presences <- terra::spatSample(as.polygons(r), 100)
set.seed(456)
absences <- terra::spatSample(as.polygons(r), 70)


# add these points to the map:

points(presences, pch = 20, cex = 0.3, col = "black")
points(absences, pch = 20, cex = 0.3, col = "white")


# use 'gridRecords' on these points:

gridded_pts <- gridRecords(rst = r, pres.coords = terra::crds(presences),
abs.coords = terra::crds(absences))

head(gridded_pts)


# map the gridded points (presences black, absences white):

points(gridded_pts[ , c("x", "y")], col = gridded_pts$presence)


# you can also do it with only presence (no absence) records
# in this case, by default (with 'absences = TRUE'),
# all pixels without presence points are returned as absences:

gridded_pres <- gridRecords(rst = r, pres.coords = terra::crds(presences))

head(gridded_pres)

plot(r)
points(presences, pch = 20, cex = 0.2, col = "black")
points(gridded_pres[ , c("x", "y")], col = gridded_pres$presence)


# with only presence (no absence) records, as in this latter case,
# you can grid records for multiple species at a time
# by adding a 'species' argument

presences$species <- rep(c("species1", "species2", "species3"), each = 33)

values(presences)

plot(r, col = hcl.colors(n = 100, palette = "blues"))
plot(presences, col = as.factor(presences$species), add = TRUE)

gridded_pres_mult <- gridRecords(rst = r, pres.coords = terra::crds(presences),
species = presences$species)

head(gridded_pres_mult)


# add each each species' gridded presences to the map:

points(gridded_pres_mult[gridded_pres_mult[ , 1] == 1, c("x", "y")], col = 1, pch = 1)
points(gridded_pres_mult[gridded_pres_mult[ , 2] == 1, c("x", "y")], col = 2, pch = 2)
points(gridded_pres_mult[gridded_pres_mult[ , 3] == 1, c("x", "y")], col = 3, pch = 3)


# a large 'rst' may cause a crash, in which case you can grid in parts:

dir.create("gridRecords_tiles")  # creates a folder to receive the tile files

terra::makeTiles(r, terra::divide(r, 2),
                 filename = "gridRecords_tiles/tile_.tif")
# give a larger 'n' to divide() if your 'rst' still crashes 'gridRecords'

tiles <- terra::sprc(list.files("gridRecords_tiles", full.names = TRUE))

par(mfrow = c(2, 2))
sapply(tiles, plot)

gridded_list <- lapply(tiles, gridRecords,
                       pres.coords = terra::crds(presences),
                       plot = TRUE)

gridded_pres <- do.call(rbind, unname(gridded_list))

unlink("gridRecords_tiles", recursive = TRUE)  # deletes the tiles folder
}
}
