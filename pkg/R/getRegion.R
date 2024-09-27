#' Get region
#'
#' @description
#' This function suggests a region for building an ecological niche model around a given set of species occurrence point coordinates. Mind that this region does not consider survey effort, geographical barriers or other factors that should also be taken into account when delimiting a region for modelling.

#' @param pres.coords [SpatVector] points with the species occurrences
#' @param type character indicating which procedure to use for defining the region. Options are:
#' - "width": a buffer whose radius is the minimum diameter of the 'pres.coords' spatial extent (computed with [terra::width()]), multiplied by 'width_mult';
#' - "mean_dist": a buffer whose radius is the mean pairwise [terra::distance()] among 'pres.coords', multiplied by 'dist_mult';
#' - "inv_dist": a buffer whose radius is the sum of the distance form each point to all other 'pres.coords';
#' - "clust_mean_dist": a different buffer around each cluster of 'pres.coords' (computed with [stats::hclust()]) and then [stats::cutree()], with the mean point distance as the 'h' parameter), sized according to the mean pairwise distance of each cluster's 'pres.coords'.
#'  - "clust_width": a different buffer around each cluster of 'pres.coords' (computed with [stats::hclust()]) and then [stats::cutree()], with the mean point distance as the 'h' parameter), sized according to the [terra::width()] of each cluster's 'pres.coords'.
#' @param width_mult if type = "width" or "clust_width", multiplier of the width to use for the [terra::buffer()] radius. Default 0.5.
#' @param dist_mult if type = "mean_dist" or "clust_mean_dist", multiplier of the mean pairwise point distance to use for the [terra::buffer()] radius around each cluster. Default 1.
#' @param verbosity integer indicating the amount of messages to display. The default is 2, for all available messages.
#' @param plot logical (default TRUE) indicating whether to plot the resulting region around 'pres.coords'
#'
#' @return SpatVector polygon
#' @author A. Marcia Barbosa
#' @seealso [terra::buffer()], [terra::width()]

#' @importFrom terra aggregate buffer crs distance is.lonlat is.points set.crs vect width
#' @importFrom stats cutree hclust
#' @export
#'
#' @examples

getRegion <- function(pres.coords,
                      type = "width",
                      clust_dist = 100,
                      dist_mult = 1,
                      width_mult = 0.5,
                      weight = TRUE,
                      CRS = NULL,
                      verbosity = 2,
                      plot = TRUE)
{

  if (!(type %in% c("width", "mean_dist", "inv_dist", "clust_mean_dist", "clust_width"))) stop("Invalid 'type'. See help file for options.")

  stopifnot(dist_mult > 0,
            width_mult > 0,
            is.numeric(verbosity),
            is.logical(plot)
  )

  if (!inherits(pres.coords, "SpatVector")) {
    pres.coords <- as.data.frame(pres.coords)
    if (ncol(pres.coords) > 2) stop("If not a SpatVector, 'pres.coords' must have only two columns")
    pres.coords <- terra::vect(pres.coords, geom = colnames(pres.coords))
  }


  if (inherits(pres.coords, "SpatVector")) {

    if (isFALSE(terra::is.points(pres.coords)))
      stop ("If 'pres.coords' is of class 'SpatVector', its 'geomtype' must be 'points'.")

    if (!is.null(CRS)) {
      if (terra::crs(pres.coords) == "") {
        terra::set.crs(pres.coords, CRS)
      } else {
        message("'CRS' argument ignored, as 'pres.coords' already has one.")
      }
    }

    #   else {
    #     if (terra::is.lonlat(pres.coords, perhaps = TRUE, warn = FALSE)) {
    #       warning("CRS not defined. Assuming EPSG:4326. Please input 'pres.coords' as a\n'SpatVector' with a properly defined CRS if this isn't the case.")
    #       terra::set.crs(pres.coords, "EPSG:4326")
    #     } else {
    #       stop ("'pres.coords' CRS not defined, and coordinates incompatible with\nlongitude-latitude degrees. Please input 'pres.coords' as a 'SpatVector'\nwith a properly defined CRS.")
    #     }
    #   }

  }  # end if SpatVector


  if (grepl("dist|clust", type)) {
    if (verbosity > 0) message("Computing pairwise distance between points...")
    dist_mat <- terra::distance(pres.coords)
    dist_mean <- mean(dist_mat, na.rm = TRUE)
  }

  if (grepl("clust", type)) {
    if (verbosity > 1) message("Computing point clusters...")

    # nrby <- nearby(pres.coords, k = 1)
    # nearest_dist <- distance(pres.coords[nrby[ , 1], ], pres.coords[nrby[ , 2], ], pairwise = TRUE)

    tree <- stats::hclust(d = dist_mat, method = "single")
    pres.coords$clust <- stats::cutree(tree, h = clust_dist * 1000)
    # clust_polygons <- terra::convHull(pres.coords, by = "clust")
    # buff_radius <- sqrt(terra::expanse(clust_polygons))
    clusters <- unique(pres.coords$clust)
    # if (verbosity > 1) message("- got ", length(clusters), " clusters")
  }


  if (type == "mean_dist") {
    reg <- terra::buffer(pres.coords, width = dist_mean * dist_mult)
  }

  else if (type == "inv_dist") {
    dist_df <- as.data.frame(as.matrix(dist_mat))
    dist_sums <- sapply(dist_mat, sum, na.rm = TRUE)  # sum of distances from each point to all other points
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    dist_sums_01 <- range01(dist_sums)
    dist_sums_01[dist_sums_01 == 0] <- 0.001  # otherwise buffer() error
    reg <- terra::buffer(pres.coords, width = dist_mean * rev(dist_sums_01) * dist_mult)
  }

  else if (type == "clust_mean_dist") {
    if (verbosity > 0) message("Computing pairwise distance within clusters...")  # before loop to avoid message repetitions
    for (i in clusters) {
      # if (verbosity > 1) cat(i, "")
      clust_pts <- pres.coords[pres.coords$clust == i, ]
      buff_radius <- mean(terra::distance(clust_pts)) * dist_mult
      if (!is.finite(buff_radius) || buff_radius <= 0) buff_radius <- 0.001  # clusters with only one point get no distance, and a zero-width buffer cannot be computed for points
      pres.coords[pres.coords$clust == i, "buff_radius"] <- buff_radius
    }

    if (isTRUE(weight)) {
      counts <- table(pres.coords$clust)
      clust_n <- counts[match(pres.coords$clust, names(counts))]
      # clust_abund <- clust_n / sum(clust_n)  # would add up to 1
      clust_abund <- (clust_n - min(clust_n)) / (max(clust_n) - min(clust_n))  # range between 0 and 1
      clust_abund[clust_abund == 0] <- 0.001  # zero buffer not allowed for points
      pres.coords$buff_radius <- pres.coords$buff_radius * clust_abund
    }

    # agg <- terra::aggregate(pres.coords, by = "clust")
    # # reg <- terra::aggregate(terra::buffer(agg, width = buff_radius))
    #
    # buffs <- vector("list", nrow(agg))
    # for (b in 1:nrow(agg)) {
    #   buffs[[b]] <- terra::buffer(agg[b, ], width = buff_radius[b])
    # }
    # reg <- terra::aggregate(do.call(rbind, buffs))

    reg <- terra::buffer(pres.coords, width = "buff_radius")
  }

  else if (type == "clust_width") {
    if (verbosity > 0) message("Computing cluster widths...")  # before loop to avoid message repetitions
    for (i in clusters) {
      clust_pts <- pres.coords[pres.coords$clust == i, ]
      clust_width <- terra::width(terra::aggregate(clust_pts)) * width_mult
      if (clust_width <= 0) clust_width <- 0.001  # negative or zero-width buffer cannot be computed for points
      pres.coords[pres.coords$clust == i, "clust_width"] <- clust_width
    }

    if (isTRUE(weight)) {
      counts <- table(pres.coords$clust)
      clust_n <- counts[match(pres.coords$clust, names(counts))]
      clust_abund <- (clust_n - min(clust_n)) / (max(clust_n) - min(clust_n))  # range between 0 and 1
      clust_abund[clust_abund == 0] <- 0.001  # zero buffer not allowed for points
      pres.coords$clust_width <- pres.coords$clust_width * clust_abund
    }

    reg <- terra::buffer(pres.coords, width = "clust_width")
  }

  else if (type == "width") {
    wdth <- terra::width(terra::aggregate(pres.coords))
    reg <- terra::buffer(pres.coords, width = wdth * width_mult)
  }


  reg <- terra::aggregate(reg)

  # if (!is.null(surveyed_polygons)) {
  #   reg <- terra::union(reg, surveyed_polygons)
  # }

  if (plot) {
    terra::plot(reg, col = "yellow", border = NA,
                main = paste("type =", type))

    if (!grepl("clust", type)) {
      terra::plot(pres.coords, cex = 0.3, add = TRUE)
    } else {
      clust_aggregates <- terra::aggregate(pres.coords, "clust")
      clust_centroids <- terra::centroids(clust_aggregates)

      # with cluster legend:
      # terra::text(clust_centroids, "agg_n", cex = 0.5)  # not all labels show if this is placed only after the following plot...
      # nc <- ifelse(length(clusters) > 10, 2, 1)
      # terra::plot(pres.coords, "clust", cex = 0.3, add = TRUE,
      #             col = grDevices::hcl.colors(length(clusters),
      #                                         palette = "dark2"))

      terra::plot(clust_aggregates, cex = 0.3, add = TRUE,
                  col = grDevices::hcl.colors(length(clusters),
                                              palette = "dark2"))
      terra::text(clust_centroids, "agg_n", cex = 0.5)
    }
  }

  if (verbosity > 1 && grepl("dist|clust", type)) message("Finished!")

  return(reg)
}
