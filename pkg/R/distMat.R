distMat <- function(coords, CRS = NULL, dist_method = "auto", verbosity = 2) {

  # version 1.2 (3 Feb 2025)

  stats_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  geodist_methods <- c("geodesic", "haversine", "vincenty", "cheap")
  terra_methods <- c("geo", "haversine", "cosine")

  # if (!(dist_method %in% c("auto", stats_methods, geodist_methods, terra_methods))) stop ("Invalid 'dist_method'")
  dist_method <- tolower(dist_method)
  dist_method <- match.arg(dist_method, unique(c("auto", stats_methods, geodist_methods, terra_methods)))


  if (is.null(CRS) || is.na(CRS) || CRS == "") {

  }



  pkgs <- .packages(all.available = TRUE)

  if (dist_method == "haversine" && !("terra" %in% pkgs || "geodist" %in% pkgs)) stop("The specified 'dist_method' requires either the 'terra' or the 'geodist' package.\nPlease install one of those, or choose a different method.")
  if (dist_method %in% setdiff(terra_methods, "haversine") && !("terra" %in% pkgs)) stop("The specified 'dist_method' requires the 'terra' package.\nPlease install it, or choose a different method.")
  if (dist_method %in% setdiff(terra_methods, c("geo", "haversine")) && utils::packageVersion("terra") < "1.8.7") stop ("The specified 'dist_method' requires 'terra' version 1.8.7 or higher.\nPlease update / (re)install 'terra', or choose a different method.")
  if (dist_method %in% setdiff(geodist_methods, "haversine") && !("geodist" %in% pkgs)) stop("The specified 'dist_method' requires the 'geodist' package.\nPlease install it, or choose a different method.")

  if ("terra" %in% pkgs) {
    if (!inherits(coords, "SpatVector")) {
      coords <- as.data.frame(coords)  # accommodate tibbles etc.
      coords <- terra::vect(coords, geom = colnames(coords), crs = ifelse(is.null(CRS), "", CRS), keepgeom = TRUE)
    } else CRS <- terra::crs(coords)
  }  # end if terra

    if (dist_method == "auto") {
      if (any(c("terra", "geodist") %in% pkgs))
      dist_method <- "haversine"
    else
      dist_method <- "euclidean"

      if ("terra" %in% pkgs && utils::packageVersion("terra") >= "1.8.7") {
        if (isTRUE(terra::is.lonlat(coords, perhaps = TRUE)))
          small <- 0.00001  # degrees
        else
          small <- 1  # meter
        min_lon_dist <- min(diff(sort(unique(terra::values(coords)[,1]))), na.rm = TRUE)
        min_lat_dist <- min(diff(sort(unique(terra::values(coords)[,2]))), na.rm = TRUE)
        if (min_lon_dist > small || min_lat_dist > small)
          dist_method <- "cosine"  # faster, but inaccurate for distances <1m (https://gis.stackexchange.com/questions/4906/why-is-law-of-cosines-more-preferable-than-haversine-when-calculating-distance-b)

      } else {  # if terra < 1.8.7
        if (verbosity > 0)  message("Faster 'auto' dist_method available with 'terra' >= 1.8.7; using slower 'geo' dist_method instead.\nUpdate / (re)install 'terra' for much faster computation.")
      }

    if (verbosity > 0) message("using '", dist_method, "' distance", sep = "")
    }  # end if "auto"

  if (dist_method %in% stats_methods) {
    if (verbosity > 1) message("Using stats::dist() for distance computation.\nResults are less accurate, especially for large distances,\nas they don't consider the curvature of the Earth.")

    coords <- as.data.frame(coords)
    return(as.matrix(stats::dist(coords, method = dist_method)))
  }


  if (dist_method %in% setdiff(geodist_methods, "haversine") && "geodist" %in% pkgs) {

    # if ("terra" %in% pkgs && isFALSE(terra::is.lonlat(coords, perhaps = TRUE)) && terra::crs(coords) != "") {
    #   coords <- terra::project(coords, "EPSG:4326")
    #   CRS <- terra::crs(coords)
    # }

    # coords <- as.data.frame(coords)
    # colnames(coords) <- c("lon", "lat")  # otherwise geodist error "Unable to determine longitude and latitude columns; perhaps try re-naming columns"
    coords <- terra::crds(coords)
    xrange <- range(coords[ , 1])
    yrange <- range(coords[ , 2])
    if (xrange[1] < -180 || xrange[2] > 180 || yrange[1] < -90 || yrange[2] > 90) warning("coordinates are out of range for geographic degrees, which are required by the specified 'dist_method' from the 'geodist' package")

    return(as.matrix(geodist::geodist(coords, measure = dist_method)))
  }  # end if geodist


  if (dist_method %in% terra_methods) {
    if (is.null(CRS) || CRS == "") {
      if (isTRUE(terra::is.lonlat(coords, perhaps = TRUE))) {
        terra::set.crs(coords, "EPSG:4326")
        if (verbosity > 0) warning("Null or empty CRS; assuming EPSG:4326.")
      } else {
        terra::set.crs(coords, "local")  # arbitrary Cartesian space
        if (verbosity > 0) warning("With null or empty CRS, distances are in the units of 'coord.cols'\nand they may be inaccurate and inconsistent across latitudes.")
      }
    }  # end if null CRS

    return(as.matrix(terra::distance(coords, method = dist_method)))
  }  # end if terra
}
