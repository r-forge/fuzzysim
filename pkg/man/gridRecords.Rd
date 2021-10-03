\name{gridRecords}
\alias{gridRecords}
\title{
Grid point occurrence records onto a raster
}
\description{
This function takes a Raster* or SpatRaster object and a set of spatial coordinates of a species' presence (and optionally absence) records, and returns a data frame with the presences and absences as well as the corresponding values of the rasters in the grid of pixels (cells). If absences = TRUE (the default) and absence coordinates are not supplied, all pixels without any presence point are returned as absences.
}
\usage{
gridRecords(rst, pres.coords, abs.coords = NULL, absences = TRUE, na.rm = TRUE)
}
\arguments{
  \item{rst}{
a Raster* or SpatRaster object (the latter is processed faster) with the desired spatial resolution and extent for the species presence-absence data, and the layer(s) whose values to extract for those data.
}
  \item{pres.coords}{
a matrix or data frame with two columns containing, respectively, the x and y, or longitude and latitude coordinates (in this order, and in the same coordinate reference system as 'rst') of the points where species presence was detected.
}
  \item{abs.coords}{
(optional) same as 'pres.coords' but for points where the species was not detected. If NULL (the default), all pixels that are not intersected by 'pres.coords' will be output as absence cells.
}
  \item{absences}{
logical value indicating whether pixels without presence records should be returned as absences. The default is TRUE.
}
  \item{na.rm}{
Logical value indicating whether pixels without values in any of the 'rst' layers should be removed from the resulting data frame. The default is TRUE.
}
}
\details{
See e.g. Baez et al. (2020), where this function was used to get unique presences and absences from point occurrence data at the spatial resolution of marine raster variables.
}
\value{
This function returns a data frame with the following columns:
\item{'presence'}{integer, 1 for the cells (pixels) with at least one presence point, and 0 for the cells without any presence point (if absences = TRUE) or with at least one absence point (if 'abs.coords' are provided) AND with no presence points}
\item{'x', 'y'}{centroid coordinates of each cell (pixel))}
\item{'cell'}{the pixel identifier in 'rst'}
\item{one column for each layer in 'rst'}{value of each pixel for each layer.}
}
\note{
This function requires either the \pkg{raster} or the \pkg{terra} package.
}
\references{
Baez J.C., Barbosa A.M., Pascual P., Ramos M.L. & Abascal F. (2020) Ensemble modelling of the potential distribution of the whale shark in the Atlantic Ocean. Ecology and Evolution, 10: 175-184
}
\author{
A. Marcia Barbosa
}
\seealso{
'extract' in package \pkg{raster} or in package \pkg{terra}
}
\examples{
\dontrun{

# you can run the following code if you have the 'raster' package installed

# import a system raster with 3 layers and crop it to a smaller extent:
require(raster)
rst <- stack(system.file("external/rlogo.grd", package = "raster"))
ext <- extent(c(0, 15, 25, 40))
rst <- crop(rst, ext)
plot(rst)
plot(rst[[1]])

# generate some random presence and absence points:
set.seed(123)
presences <- sp::spsample(as(ext, "SpatialPolygons"), 50, type = "random")
absences <- sp::spsample(as(ext, "SpatialPolygons"), 50, type = "random")
points(presences, pch = 20, cex = 0.2, col = "black")
points(absences, pch = 20, cex = 0.2, col = "white")

# use 'gridRecords' on these random points:
gridded_pts <- gridRecords(rst, coordinates(presences), coordinates(absences))
head(gridded_pts)  # 'red', 'green' and 'blue' are the names of the layers in 'rst'

# plot them to check the result:
pres_coords <- gridded_pts[gridded_pts$presence == 1, c("x", "y")]
abs_coords <- gridded_pts[gridded_pts$presence == 0, c("x", "y")]
points(gridded_pts[ , c("x", "y")], pch = 4, cex = 0.6, col = gridded_pts$presence)

# you can also do it with only presence (no absence) records:
gridded_pres <- gridRecords(rst, coordinates(presences))
head(gridded_pres)
plot(rst[[1]])
points(presences, pch = 20, cex = 0.2, col = "black")
pres_coords <- gridded_pres[gridded_pres$presence == 1, c("x", "y")]
abs_coords <- gridded_pres[gridded_pres$presence == 0, c("x", "y")]
points(gridded_pres[ , c("x", "y")], pch = 4, cex = 0.6, col = gridded_pres$presence)
}
}
