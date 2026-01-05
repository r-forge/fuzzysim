\name{distMat}
\alias{distMat}
\title{Distance matrix for spatial coordinates}
\description{
  Computes a distance matrix for a given set of spatial coordinates using a specified distance measure.
}
\usage{
  distMat(coords, CRS = NULL, method = "auto", verbosity = 2)
}
\arguments{
  \item{coords}{data frame (or an object that can be coerced to such) with only two columns containing the spatial coordinates (x and y, or longitude and latitude, or easting and northing, in this order!)}
  \item{CRS}{Coordinate Reference System for 'coords' (if it is not a SpatVector with its CRS defined already), in one of the following formats: WKT/WKT2, <authority>:<code>, or PROJ-string notation (see \code{\link[terra:crs]{terra::crs()}}). If provided and either the 'geodist' or the 'terra' package is installed, distances are computed with \code{\link[geodist:geodist]{geodist::geodist()}} or with \code{\link[terra:distance]{terra::distance()}}, thus accounting for the curvature of the Earth. If not provided and coordinates fall within valid latitude-longitude bounds, EPSG:4326 is assumed.}
  \item{method}{The method to use for distance calculation. Partial and case-insensitive argument matching is used. Options are:
    \itemize{
      \item{"auto" (the default): the recommended method under the circumstances (see Details).}
      \item{"euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski": older methods, implemented in \code{\link[stats:dist]{stats::dist()}}, which do not acocunt for the curvature of the Earth}
      \item{"geodesic", "haversine" "vincenty", "cheap": methods implemented in \code{\link[geodist:geodist]{geodist::geodist()}} (note this requires that 'coords' are in geographic degrees)}
      \item{"geo", "haversine", "cosine": methods implemented in \code{\link[terra:distance]{terra::distance()}}}
  }
  If a metric has the same name in 'terra' and 'geodist' and both packages are available, the latter is used, as it is slightly faster.
}
  \item{verbosity}{integer specifying the amount of messages to display along the process. The default is 2, for the maximum amount of messages available.}
}
\details{
  This function computes a matrix of pairwise distances for the input set of coordinates. It supports various distance measures by calling \code{\link[stats:dist]{stats::dist()}}, \code{\link[terra:distance]{terra::distance()}} or \code{\link[geodist:geodist]{geodist::geodist()}}, depending on the specified method and on the installed R packages.

  With the default "auto" option, if neither \code{terra} nor \code{geodist} are installed, the method is "euclidean" from \code{\link[stats:dist]{stats::dist()}}. Otherwise, the method is "haversine" (relatively fast, but may be inaccurate at long distances, especially near the poles) if there are distances < 1 m or 0.00001 degrees; or "cosine" (faster but inaccurate for small distances) otherwise (unless 'terra' version is older than 1.8.7, when "cosine" was implemented). You can instead choose a specific option, e.g. "geo" or "geodesic" for the most accurate yet slowest distance method (see ?terra::distance; may crash if many points); "vincenty" (more accurate than "haversine" as it considers the Earth's ellipticity, but slower and may fail for nearly antipodal points); or "cheap" (the fastest, but inaccurate for maximum distances >100 km).
}
\value{
  A matrix of distances between the coordinates.
}
\author{
  A. Marcia Barbosa
}
\examples{
  # generate some random geographical coordinates:

  n_points <- 10

  set.seed(123)
  coords <- matrix(c(runif(n_points, min = -180, max = 180),
                     runif(n_points, min = -90, max = 90)),
                     ncol = 2)


  # compute distance matrix (may fail with outdated PROJ version):

  \dontrun{
    distMat(coords, method = "euclidean")

    distMat(coords, method = "auto")

    distMat(coords, method = "auto", CRS = "EPSG:4326")
  }
}
