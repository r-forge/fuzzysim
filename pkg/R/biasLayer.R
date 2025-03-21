biasLayer <- function(svc, rst = terra::rast(terra::ext(svc), crs = terra::crs(svc)), type = "distance", combine = "sum", ...) {
  # version 1.0 (24 Feb 2025)

  # tentative arguments:
  # inv = TRUE  # for distance
  # standardize = TRUE  # for standardizing all to vary between 0 and 1

  if (!inherits(svc, "SpatVectorCollection")) stop("'svc' must be of class 'SpatVectorCollection';\n  try converting it with terra::svc()")
  if (!inherits(rst, "SpatRaster")) stop("'rst' must be of class 'SpatRaster'")
  type <- match.arg(type, c("distance", "rasterize"))

  if (!terra::same.crs(svc, rst))
    svc <- terra::project(svc, rst)

  n_maps <- length(svc)
  rasts <- vector("list", n_maps)

  if (type == "distance") {
    for (i in 1:n_maps) {
      message("computing distance for map ", i, " of ", n_maps, "...")
      r <- terra::rasterize(svc[[i]], rst, field = 1)
      rasts[[i]] <- terra::distance(r, ...)
    }
  }

  if (type == "rasterize") {
    for (i in 1:n_maps) {
      message("rasterizing map ", i, " of ", n_maps, "...")
      rasts[[i]] <- terra::rasterize(svc[[i]], rst, background = 0, ...)
    }
  }

  # if (standardize) rasts <- lapply(rasts, function(x) terra::app(x, modEvA::range01, na.rm = TRUE))

  rasts <- do.call(c, rasts)

  # if (standardize) {
  #   rasts <- modEvA::range01(rasts)
  #   rasts <- terra::app(rasts, modEvA::range01)
  #   rasts <- terra::lapp(rasts, modEvA::range01)

  for (i in 1:terra::nlyr(rasts)) {
    rasts[[i]] <- terra::app(rasts[[i]], modEvA::range01)
  }

  # if (inv && type == "distance") rasts <- 1 - rasts
  if (type == "distance") rasts <- 1 - rasts

  # }

  out <- terra::app(rasts, fun = combine)
  out <- out - terra::minmax(out)["min", ]  # bias layer must not have negative values
  return(out)
}
