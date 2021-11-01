gridRecords <- function(rst,
                        pres.coords,
                        abs.coords = NULL,
                        absences = TRUE,
                        na.rm = TRUE) {
  
  # version 2.2 (6 Oct 2020)
  
  if (!requireNamespace("raster", quietly = TRUE) && !requireNamespace("terra", quietly = TRUE)) stop("This function requires having either the 'raster' or the 'terra' package installed.")
  
  if (inherits(rst, "Raster")) {
    #if (requireNamespace("terra"))  rst <- terra::rast(rst)  # for faster processing
    #else {
      p_extract <- raster::extract(rst, pres.coords, cellnumbers = TRUE, df = TRUE)[ , -1]
      p_extract <- unique(p_extract)
      p_centroids <- raster::xyFromCell(rst, p_extract$cells)
      p_extract <- data.frame(presence = 1, p_centroids, p_extract)
      
      if (absences == TRUE) {
        if (is.null(abs.coords)) {
          abs.coords <- raster::coordinates(rst)
        }  # presence coordinates removed below
        a_extract <- raster::extract(rst, abs.coords, cellnumbers = TRUE, df = TRUE)[ , -1]
        a_extract <- unique(a_extract)
        a_extract <- a_extract[!(a_extract$cells %in% p_extract$cells), ]
        a_centroids <- raster::xyFromCell(rst, a_extract$cells)
        if (nrow(a_extract) > 0) {
          a_extract <- data.frame(presence = 0, a_centroids, a_extract)
        }
      } else {
        a_extract <- NULL
      }
    #}  # end if !terra
  }  # end if Raster*
  
  else if (inherits(rst, "SpatRaster")) {
    p_extract <- terra::extract(rst, pres.coords, cells = TRUE, xy = FALSE)
    p_extract$ID <- NULL
    p_extract <- unique(p_extract)
    p_centroids <- terra::xyFromCell(rst, p_extract$cell)
    p_extract <- data.frame(presence = 1, p_centroids, p_extract[ , "cell", drop = FALSE], p_extract[ , 1:terra::nlyr(rst)])
    
    if (absences == TRUE) {
      if (is.null(abs.coords)) {
        #abs.coords <- coordinates(rst)
        abs.coords <- terra::geom(terra::centroids(terra::as.points(rst)))[ , c("x", "y")]
        #abs.coords <- terra::xyFromCell(rst, cells(rst))
      }  # presence coordinates removed below
      a_extract <- terra::extract(rst, abs.coords, cells = TRUE, xy = TRUE)
      a_extract$ID <- NULL
      a_extract <- unique(a_extract)
      a_extract <- a_extract[!(a_extract$cell %in% p_extract$cell), ]
      if (nrow(a_extract) > 0) {
        a_extract <- data.frame(presence = 0, a_extract[ , c("x", "y", "cell")], a_extract[ , 1:terra::nlyr(rst)])
      }
    } else {
      a_extract <- NULL
    }
  }  # end if SpatRaster
  
  result <- rbind(p_extract, a_extract)
  
  if (na.rm) {
    result_NA <- which(apply(result[ , 5:ncol(result), drop = FALSE], MARGIN = 1, FUN = function(x) all(is.na(x))))
    if (length(result_NA) > 0) {
      result <- result[-result_NA, ]
    }
  }

  colnames(result) <- sub("cell$", "cells", colnames(result))  # for uniformity between 'raster' and 'terra' output
  
  return(result)
}
