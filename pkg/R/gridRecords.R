gridRecords <- function(rst,
                        pres.coords,
                        abs.coords = NULL,
                        absences = TRUE,
                        na.rm = TRUE) {

  # version 2.1 (10 Sep 2020)

  if (!requireNamespace("raster")) stop("This function requires installing the 'raster' package first.")

  p_extract <- raster::extract(rst, pres.coords, cellnumbers = TRUE, df = TRUE)[ , -1]
  p_extract <- unique(p_extract)
  p_centroids <- raster::xyFromCell(rst, p_extract$cells)
  p_extract <- data.frame(presence = 1, p_centroids, p_extract)

  if (absences == TRUE) {
    if (is.null(abs.coords)) {
      abs.coords <- raster::coordinates(rst)
    }
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

  result <- rbind(p_extract, a_extract)

  if (na.rm) {
    result_NA <- which(apply(result[ , 5:ncol(result)], MARGIN = 1, FUN = function(x) all(is.na(x))))
    if (length(result_NA) > 0) {
      result <- result[-result_NA, ]
    }
  }

  return(result)
}
