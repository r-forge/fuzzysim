\name{biasLayer}
\alias{biasLayer}
\title{Compute a bias layer}
\description{
  Computes a bias layer based on a SpatVectorCollection of lines, points and/or polygons, using either distance or rasterizing.
}
\usage{
  biasLayer(svc, rst = terra::rast(terra::ext(svc)), type = "distance",
  combine = "sum", ...)
}
\arguments{
  \item{svc}{An object of class \code{SpatVectorCollection}.}
  \item{rst}{A \code{SpatRaster} with the desired dimensions and CRS for the output. The default has the extent of 'svc' and the default arguments for terra::rast(), but ideally the user should provide a template raster with the desired dimensions.}
  \item{type}{A character string indicating the method to use. Options are \code{"distance"} (the default) or \code{"rasterize"}.}
  \item{combine}{A character string to pass as the 'fun' argument to terra::app(), indicating the method for combining the resulting raster layers into one. Default is \code{"sum"}.}
  \item{...}{Additional arguments passed to \code{terra::distance} or to \code{terra::rasterize}, depending on 'type'.}
}
\details{
  This function can produce a bias layer to use e.g. as the 'bias' argument in \code{\link{selectAbsences}}. The input points, lines and/or polygons should reflect aeas where survey is more likely for the target species. They can be e.g. roads or other access pathways; natural parks or other usually surveyed areas; occurrence records of other species of the same taxon or guild; and/or rivers and streams, namely for freshwater species.

  The function calculates the bias layer by either computing the inverse standardized distance (i.e., one minus modEvA::range01(distance)) from the each of the features in 'svc', or by rasterizing those features (depending on the input 'type'). It then combines the layers into one, using the specified 'combine' method.
}
\value{
  A \code{SpatRaster}.
}
\examples{
  \dontrun{
    # get some example layers:

    lux <- terra::vect(system.file("ex/lux.shp", package = "terra"))
    poly <- lux[2, ]
    roads <- geodata::osm("Luxembourg", var = "highways", path = tempdir())
    primary_roads <- terra::subset(roads, roads$highway == "primary")
    asphalt_roads <- terra::subset(roads, roads$highway == "primary" | roads$surface == "asphalt")


    # get raster template for extent and resolution:

    elev <- terra::rast(system.file("ex/elev.tif", package = "terra"))


    # plot layers:

    plot(elev)
    plot(poly, col = "darkgreen", add = TRUE)
    plot(asphalt_roads, lwd = 0.3, add = TRUE)
    plot(primary_roads, add = TRUE)


    # compute bias layer based on these:

    bias_dist <- biasLayer(terra::svc(poly, asphalt_roads, primary_roads), rst = elev, type = "distance")

    bias_rast <- biasLayer(terra::svc(poly, primary_roads, asphalt_roads), rst = elev, type = "rasterize", background = 0)


    bias <- bias_rast

    plot(bias, col = map.pal("magma"), main = "bias layer")
    plot(poly, density = 60, col = "darkgreen", lwd = 2, add = TRUE)
    plot(asphalt_roads, col = "royalblue", lwd = 0.4, add = TRUE)
    plot(primary_roads, col = "green", lwd = 1.2, add = TRUE)
  }
}
\author{
  A. Marcia Barbosa
}
