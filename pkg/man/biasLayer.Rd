\name{biasLayer}
\alias{biasLayer}
\title{Compute a bias layer}
\description{
  Computes a bias layer based on a \code{\link[terra:SpatVectorCollection]{terra::SpatVectorCollection}} of lines, points and/or polygons that may indicate better surveyed areas, using either distance or rasterizing.
}
\usage{
  biasLayer(svc, rst = terra::rast(terra::ext(svc), crs = terra::crs(svc)),
  type = "distance", combine = "sum", ...)
}
\arguments{
  \item{svc}{An object of class \code{\link[terra:SpatVectorCollection]{terra::SpatVectorCollection}}.}
  \item{rst}{A \code{\link[terra:SpatRaster]{terra::SpatRaster}} with the desired dimensions and CRS for the output. The default has the spatial extent of 'svc' and the default arguments for \code{\link[terra:rast]{terra::rast()}}, but ideally the user should provide a template raster with the desired dimensions.}
  \item{type}{A character string indicating the method to use. Options are inverse "distance" (the default, with higher values indicating closer proximity to the input features, or more likely survey); or "rasterize" (where only pixels overlapping the input features are given values higher than zero, for possible survey).}
  \item{combine}{A character string to pass as the 'fun' argument to \code{\link[terra:app]{terra::app()}}, indicating the method for combining the resulting raster layers into one. The default is \code{"sum"}.}
  \item{...}{Additional arguments passed to \code{\link[terra:distance]{terra::distance()}} or to \code{\link[terra:rasterize]{terra::rasterize()}}, depending on 'type'.}
}
\details{
  This function can produce a bias layer to use e.g. as the 'bias' argument in \code{\link{selectAbsences}}. The input points, lines and/or polygons should reflect areas or features where survey is possible (if type = "rasterize") or generally more likely (if type = "distance") for the target species. They can be e.g. roads or other access pathways; natural parks or other usually surveyed areas; occurrence records of the target and/or other species of the same taxon or guild; and/or rivers and streams, namely for freshwater species.

  The function calculates the bias layer by either computing the inverse standardized distance, i.e. one minus \code{\link[modEvA:range01]{modEvA::range01(}}distance), to the each of the features in 'svc'; or by rasterizing those features (depending on 'type'). It then combines the resulting raster layers into one, using the specified 'combine' method.
}
\value{
  A \code{\link[terra:SpatRaster]{terra::SpatRaster}}.
}
\examples{
  \dontrun{
    # load mapping packages:

    library(terra)
    library(geodata)


    # get some example layers:

    lux <- vect(system.file("ex/lux.shp", package = "terra"))
    poly <- lux[2, ]
    roads <- osm("Luxembourg", var = "highways", path = tempdir())
    primary_roads <- subset(roads, roads$highway == "primary")
    asphalt_roads <- subset(roads, roads$highway == "primary" | roads$surface == "asphalt")


    # get raster template for extent and resolution:

    elev <- rast(system.file("ex/elev.tif", package = "terra"))


    # plot the layers:

    plot(elev)
    plot(poly, col = "darkgreen", add = TRUE)
    plot(asphalt_roads, lwd = 0.3, add = TRUE)
    plot(primary_roads, add = TRUE)


    # compute and plot bias layers based on these:

    bias_dist <- biasLayer(svc(poly, asphalt_roads, primary_roads),
    rst = elev, type = "distance")

    bias_rast <- biasLayer(svc(poly, primary_roads, asphalt_roads),
    rst = elev, type = "rasterize")

    plot(bias_dist, col = map.pal("plasma"), main = "bias layer: distance")
    plot(poly, density = 20, add = TRUE)
    plot(asphalt_roads, lwd = 0.5, add = TRUE)
    plot(primary_roads, lwd = 1.2, add = TRUE)

    plot(bias_rast, col = map.pal("plasma"), main = "bias layer: rasterize")
    plot(poly, density = 20, add = TRUE)
    plot(asphalt_roads, lwd = 0.5, add = TRUE)
    plot(primary_roads, lwd = 1.2, add = TRUE)
  }
}
\author{
  A. Marcia Barbosa
}
