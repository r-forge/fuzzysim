\name{gridRecords}
\alias{gridRecords}
\title{
Get species occurrence records gridded onto a raster
}
\description{
This function takes a raster stack and a set of geographic coordinates of a species' presence (and optionally absence) records, and returns a table with the presences and absences and the values of the rasters in the grid of pixels. If absence coordinates are not supplied, all pixels without any presence point will be returned as absences.
}
\usage{
gridRecords(rst, pres.coords, abs.coords = NULL)
}
\arguments{
  \item{rst}{
a Raster* object with the desired spatial resolution and extent for the species presence-absence data. The raster should be masked so that pixels have values only in relevant and reasonably surveyed areas.
}
  \item{pres.coords}{
a matrix or data frame with two columns containing, respectively, the x and y, or longitude and latitude coordinates (in this order, and in the same coordinate reference system as 'rst') of the points where species presence was detected.
}
  \item{abs.coords}{
same as 'pres.coords' but for points where the species was not detected. If kept NULL, all pixels with a finite value that are not intersected by 'pres.coords' will be output as absence grid cells.
}
}
\details{
This function was used e.g. in Baez et al. (2020) to get unique presences and absences from point occurrence data onto raster pixels.
}
\value{
This function returns a data frame with the following columns:
- 'presence': 1 for the cells with at least one presence point, and 0 for the cells with absence points (if provided) AND with no presence points
- 'x', 'y': centroid coordinates of each pixel
- 'cellnumber': the pixel identifier in 'rst'
- one column for each variable in 'rst', with the value of each pixel for that variable.
}
\references{
Baez J.C., Barbosa A.M., Pascual P., Ramos M.L. & Abascal F. (2020) Ensemble modelling of the potential distribution of the whale shark in the Atlantic Ocean. Ecology and Evolution, 10: 175-184
}
\author{
A. Marcia Barbosa
}
\seealso{
'extract' in package \pkg{raster}
}
\examples{
}
