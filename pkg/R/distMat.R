distMat <- function(coords, CRS = NULL, method = "auto", verbosity = 2) {

  # version 1.3 (5 Jan 2026)

  stats_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  geomethods <- c("geodesic", "haversine", "vincenty", "cheap")
  terra_methods <- c("geo", "haversine", "cosine")

  # if (!(method %in% c("auto", stats_methods, geomethods, terra_methods))) stop ("Invalid 'method'")
  method <- tolower(method)
  method <- match.arg(method, unique(c("auto", stats_methods, geomethods, terra_methods)))


  # if (is.null(CRS) || is.na(CRS) || CRS == "") {
  #
  # }



  pkgs <- .packages(all.available = TRUE)

  if (method == "haversine" && !("terra" %in% pkgs || "geodist" %in% pkgs)) stop("The specified 'method' requires either the 'terra' or the 'geodist' package.\nPlease install one of those, or choose a different method.")
  if (method %in% setdiff(terra_methods, "haversine") && !("terra" %in% pkgs)) stop("The specified 'method' requires the 'terra' package.\nPlease install it, or choose a different method.")
  if (method %in% setdiff(terra_methods, c("geo", "haversine")) && utils::packageVersion("terra") < "1.8.7") stop ("The specified 'method' requires 'terra' version 1.8.7 or higher.\nPlease update / (re)install 'terra', or choose a different method.")
  if (method %in% setdiff(geomethods, "haversine") && !("geodist" %in% pkgs)) stop("The specified 'method' requires the 'geodist' package.\nPlease install it, or choose a different method.")

  if ("terra" %in% pkgs) {
    if (!inherits(coords, "SpatVector")) {
      coords <- as.data.frame(coords)  # accommodate tibbles etc.
      coords <- terra::vect(coords, geom = colnames(coords), crs = ifelse(is.null(CRS), "", CRS), keepgeom = TRUE)
    } else CRS <- terra::crs(coords)
  }  # end if terra

    if (method == "auto") {
      if (any(c("terra", "geodist") %in% pkgs))
      method <- "haversine"
    else
      method <- "euclidean"

      if ("terra" %in% pkgs && utils::packageVersion("terra") >= "1.8.7") {
        if (isTRUE(terra::is.lonlat(coords, perhaps = TRUE, warn = FALSE)))
          small <- 0.00001  # degrees
        else
          small <- 1  # meter
        min_lon_dist <- min(diff(sort(unique(terra::crds(coords)[,1]))), na.rm = TRUE)
        min_lat_dist <- min(diff(sort(unique(terra::crds(coords)[,2]))), na.rm = TRUE)
        if (min_lon_dist > small || min_lat_dist > small)
          method <- "cosine"  # faster, but inaccurate for distances <1m (https://gis.stackexchange.com/questions/4906/why-is-law-of-cosines-more-preferable-than-haversine-when-calculating-distance-b)

      } else {  # if terra < 1.8.7
        if (verbosity > 0)  message("Faster 'auto' method available with 'terra' >= 1.8.7.\nUsing slower 'geo' method instead.\nUpdate or (re)install 'terra' for much faster computation.")
      }

    if (verbosity > 0) message("  -> using '", method, "' distance", sep = "")
    }  # end if "auto"

  if (method %in% stats_methods) {
    if (verbosity > 1) message("Using stats::dist() for distance computation.\nResults are less accurate, notably for large distances,\nas they don't consider the curvature of the Earth.")

    coords <- as.data.frame(coords)
    return(as.matrix(stats::dist(coords, method = method)))
  }


  if (method %in% setdiff(geomethods, "haversine") && "geodist" %in% pkgs) {

    # if ("terra" %in% pkgs && isFALSE(terra::is.lonlat(coords, perhaps = TRUE)) && terra::crs(coords) != "") {
    #   coords <- terra::project(coords, "EPSG:4326")
    #   CRS <- terra::crs(coords)
    # }

    # coords <- as.data.frame(coords)
    # colnames(coords) <- c("lon", "lat")  # otherwise geodist error "Unable to determine longitude and latitude columns; perhaps try re-naming columns"
    coords <- terra::crds(coords)
    xrange <- range(coords[ , 1])
    yrange <- range(coords[ , 2])
    if (xrange[1] < -180 || xrange[2] > 180 || yrange[1] < -90 || yrange[2] > 90) warning("coordinates are out of range for geographic degrees, which are required by the specified 'method' from the 'geodist' package")

    return(as.matrix(geodist::geodist(coords, measure = method)))
  }  # end if geodist


  if (method %in% terra_methods) {
    if (is.null(CRS) || CRS == "") {

      # to avoid pkg check errors in R older versions, arising from:
      # Warning: PROJ: proj_create_from_database: Cannot find proj.db (GDAL error 1)
      # Warning: [crs<-] Cannot set SRS to vector: empty srs
      plib <- Sys.getenv("PROJ_LIB")
      prj <- system.file("proj", package = "terra")[1]
      Sys.setenv("PROJ_LIB" = prj)
      on.exit(Sys.setenv("PROJ_LIB" = plib))
      # https://github.com/rspatial/terra/issues/1378#issuecomment-1864893650

      if (isTRUE(terra::is.lonlat(coords, perhaps = TRUE, warn = FALSE))) {
        terra::set.crs(coords, "EPSG:4326")
        if (verbosity > 0) warning("Null or empty CRS; assuming EPSG:4326.")
      } else {
        terra::set.crs(coords, "local")  # arbitrary Cartesian space
        if (verbosity > 0) warning("With null or empty CRS, distances are in the units of 'coord.cols'\nand they may be inaccurate and inconsistent across latitudes.")
      }
    }  # end if null CRS

    return(as.matrix(terra::distance(coords, method = method)))
  }  # end if terra
}
