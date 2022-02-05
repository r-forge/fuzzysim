gridRecords <- function(rst,
                        pres.coords,
                        abs.coords = NULL,
                        absences = TRUE,
                        species = NULL,  # new
                        na.rm = TRUE) {

  # version 3.0 (31 Jan 2022)

  if (!requireNamespace("raster", quietly = TRUE) && !requireNamespace("terra", quietly = TRUE)) stop("This function requires having either the 'raster' or the 'terra' package installed.")

  pres.coords <- as.data.frame(pres.coords)
  if (!is.null(abs.coords))  abs.coords <- as.data.frame(abs.coords)

  if (!is.null(species)) {
    if (length(species) != nrow(pres.coords)) stop ("'species' must have the same length as nrow(pres.coords)")
    if (!is.null(abs.coords))  stop("Sorry, 'abs.coords' is currently only implemented for one species at a time, i.e. when species=NULL.")
    species_list <- unique(species)
    pres.coords.in <- pres.coords
    pres.coords <- pres.coords.in[species == species_list[1], ]
  }

  if (inherits(rst, "Raster")) {
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
    # end if Raster*
  } else {
    if (inherits(rst, "SpatRaster")) {
      p_extract <- terra::extract(rst, pres.coords, cells = TRUE, xy = FALSE)[ , -1]
      p_extract <- unique(p_extract)
      p_centroids <- terra::xyFromCell(rst, p_extract$cell)
      p_extract <- data.frame(presence = 1, p_centroids, p_extract[ , "cell", drop = FALSE], p_extract[ , 1:terra::nlyr(rst), drop = FALSE])

      if (absences == TRUE) {
        if (is.null(abs.coords)) {
          abs.coords <- terra::crds(rst)
          #abs.coords <- terra::xyFromCell(rst, cells(rst))
        }  # presence coordinates removed below
        a_extract <- terra::extract(rst, abs.coords, cells = TRUE, xy = FALSE)[ , -1]
        a_extract <- unique(a_extract)
        a_extract <- a_extract[!(a_extract$cell %in% p_extract$cell), ]
        a_centroids <- terra::xyFromCell(rst, a_extract$cell)
        if (nrow(a_extract) > 0) {
          a_extract <- data.frame(presence = 0, a_centroids, a_extract[ , "cell", drop = FALSE], a_extract[ , 1:terra::nlyr(rst), drop = FALSE])
        }
      } else {
        a_extract <- NULL
      }
    } # end if SpatRaster
  } # end if-else

  result <- rbind(p_extract, a_extract)
  colnames(result) <- sub("cell$", "cells", colnames(result))  # for uniformity between 'raster' and 'terra' output

  if (na.rm) {
    result_NA <- which(apply(result[ , 5:ncol(result), drop = FALSE], MARGIN = 1, FUN = function(x) all(is.na(x))))
    if (length(result_NA) > 0) {
      result <- result[-result_NA, ]
    }
  }

  if (!is.null(species)) {
    species_result <- result[ , "cells", drop = FALSE]
    for (s in species_list[-1]) {  # species 1 already gridded above
      if (inherits(rst, "Raster"))  species_cells <- raster::cellFromXY(rst, as.matrix(pres.coords.in[species == s, ]))
      if (inherits(rst, "SpatRaster"))  species_cells <- terra::cellFromXY(rst, as.matrix(pres.coords.in[species == s, ]))
      species_result[ , s] <- 0
      species_result[result$cells %in% species_cells, s] <- 1
    }

    result <- data.frame(result[ , 1, drop = FALSE],  # "presence"
                         species_result[ , -1],  # except "cells"
                         result[ , -1]  # except "presence"
    )
    names(result)[1] <- species_list[1]
  }

  return(result)
}
