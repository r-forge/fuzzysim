gridRecords <- function(rst,
                        pres.coords,
                        abs.coords = NULL,
                        absences = TRUE,
                        species = NULL,  # new
                        na.rm = TRUE,  # new
                        plot = FALSE)  # new
{

  # version 3.91 (23 Sep 2025)

  if (!("raster" %in% .packages(all.available = TRUE)) && !("terra" %in% .packages(all.available = TRUE))) stop("This function requires having either the 'raster' or the 'terra' package installed.")

  sv_input <- inherits(pres.coords, "SpatVector")

  if (sv_input) {
    if (isFALSE(terra::is.points(pres.coords))) stop ("If 'pres.coords' is of class 'SpatVector', its 'geomtype' must be 'points'.")
    pres.coords <- terra::crds(pres.coords)
  }
  pres.coords <- as.data.frame(pres.coords)  # also for tibbles etc.

  if (!is.null(abs.coords)) {
    if (inherits(abs.coords, "SpatVector")) {
      if (isFALSE(terra::is.points(abs.coords))) stop ("If 'abs.coords' is of class 'SpatVector', its 'geomtype' must be 'points'.")
      abs.coords <- terra::crds(abs.coords)
    }
    abs.coords <- as.data.frame(abs.coords)  # also for tibbles etc.
  }

  if (!is.null(species)) {
    if (length(species) != nrow(pres.coords)) stop ("'species' must have the same length as nrow(pres.coords)")
    if (!is.null(abs.coords)) stop ("Sorry, 'abs.coords' is currently only implemented for one species at a time, i.e. when species=NULL")
    #if (suppressWarnings(any(is.finite(as.numeric(species))))) warning ("'species' are used as column names, so they should be of class 'character' and not start with a number.")
    if (length(unique(species)) != length(unique(trimws(species)))) warning ("Some values in 'species' have leading or trailing spaces and are thus treated separately; consider using trimws() first.")

    species_list <- unique(species)
    pres.coords.in <- pres.coords
    pres.coords <- pres.coords.in[species == species_list[1], ]
  }

  if (inherits(rst, "Raster")) {
    p_extract <- unique(raster::extract(rst, pres.coords, cellnumbers = TRUE, df = TRUE)[ , -1])
    p_centroids <- raster::xyFromCell(rst, p_extract$cells)
    p_extract <- data.frame(presence = 1, p_centroids, p_extract)

    if (absences == TRUE) {
      if (is.null(abs.coords)) {
        abs.coords <- raster::coordinates(rst)
      }  # presence coordinates removed below
      a_extract <- unique(raster::extract(rst, abs.coords, cellnumbers = TRUE, df = TRUE)[ , -1])
      # a_extract <- a_extract[!(a_extract$cells %in% p_extract$cells), ]
      exclude_cells <- setdiff(a_extract$cells, p_extract$cells)
      a_extract <- a_extract[match(exclude_cells, a_extract$cells), , drop = FALSE]
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
      p_extract <- unique(terra::extract(rst, pres.coords, cells = TRUE, xy = FALSE)[ , -1])
      p_centroids <- terra::xyFromCell(rst, p_extract$cell)
      p_extract <- data.frame(presence = 1, p_centroids, p_extract[ , "cell", drop = FALSE], p_extract[ , 1:terra::nlyr(rst), drop = FALSE])

      if (absences == TRUE) {

        if (is.null(abs.coords)) {
          # abs.coords <- terra::crds(rst, df = TRUE, na.rm = FALSE)  # added na.rm
          abs.coords <- data.frame(terra::xyFromCell(rst, terra::cells(rst)))  # usually faster than crds()
        }  # presence coordinates removed below

        a_extract <- unique(terra::extract(rst, abs.coords, cells = TRUE, xy = FALSE)[ , -1])
        # a_extract <- a_extract[!(a_extract$cell %in% p_extract$cell), ]
        exclude_cells <- setdiff(a_extract$cell, p_extract$cell)
        a_extract <- a_extract[match(exclude_cells, a_extract$cell), , drop = FALSE]
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

  # if (na.rm) {
  #   result_NA <- which(apply(result[ , 5:ncol(result), drop = FALSE], MARGIN = 1, FUN = function(x) all(is.na(x))))
  #   if (length(result_NA) > 0) {
  #     result <- result[-result_NA, ]
  #   }
  # }

  if (na.rm) {
    # result <- result[!apply(is.na(result[ , 5:ncol(result), drop = FALSE]), 1, all), ]  # 5:ncol(result)  # names(rst)
    na_relevant <- result[ , 5:ncol(result), drop = FALSE]
    result <- result[rowSums(!is.na(na_relevant)) > 0, ]  # rowSums faster than apply
  }

  if (!is.null(species)) {

    if (plot) {
      plot <- FALSE  # otherwise Error: [plot] SpatVector has zero geometries
      message("'plot' currently incompatible with using 'species'.")
    }

    species_result <- result[ , "cells", drop = FALSE]

    for (s in species_list[-1]) {  # species 1 already gridded above
      if (inherits(rst, "Raster"))  species_cells <- raster::cellFromXY(rst, as.matrix(pres.coords.in[species == s, ]))
      if (inherits(rst, "SpatRaster"))  species_cells <- terra::cellFromXY(rst, as.matrix(pres.coords.in[species == s, ]))
      species_result[ , s] <- 0
      # species_result[result$cells %in% species_cells, s] <- 1
      species_result[na.omit(match(species_cells, result$cells)), s] <- 1  # slightly faster than %in%; but without na.omit, "Error in `[<-.data.frame`(`*tmp*`, match(species_cells, result$cells), : missing values are not allowed in subscripted assignments of data frames. Called from: `[<-.data.frame`(`*tmp*`, match(species_cells, result$cells), s, value = 1)
    }  # end for species

    result <- data.frame(result[ , 1, drop = FALSE],  # "presence"
                         species_result[ , -1, drop = FALSE],  # except "cells"
                         result[ , -1, drop = FALSE],  # except "presence"
                         check.names = FALSE  # NEW
    )
    names(result)[1] <- species_list[1]  # species 1 gridded before as "presence"
  }  # end if !null species

  result <- result[order(result$cell), ]  # new
  #result <- result[order(as.integer(rownames(result))), ]  # new
  rownames(result) <- NULL  # new

  if (plot) {
    if ("terra" %in% .packages(all.available = TRUE)) {
      result_sv <- terra::vect(result, geom = c("x", "y"), keepgeom = TRUE)  # for better-shaped plot
      terra::plot(result_sv[result_sv$presence == 0, ],
                  ext = terra::ext(result_sv),
                  pch = "-", col = "red")
      terra::points(result_sv[result_sv$presence == 1, ],
                    pch = "+", col = "blue")
      if (sv_input) result <- result_sv
    } else {  # non-spatial plot
      xrange <- range(result$x, na.rm = TRUE)
      yrange <- range(result$y, na.rm = TRUE)
      plot(result[result$presence == 0, c("x", "y")],
           xlim = xrange, ylim = yrange,
           pch = "-", col = "red")
      points(result[result$presence == 1, c("x", "y")],
             pch = "+", col = "blue")
    }
  }

  return(result)
}
