getRegion <- function(pres.coords,
                      type = "width",
                      clust_dist = 100,
                      clust_type = "buffer",
                      dist_mult = 1,
                      width_mult = 0.5,
                      weight = FALSE,
                      CRS = NULL,
                      dist_mat = NULL,
                      dist_method = "auto",
                      verbosity = 2,
                      plot = TRUE,
                      ...)
{

  # version 1.5 (1 Oct 2025)

  if (!("terra" %in% .packages(all.available = TRUE))) stop("This function requires the 'terra' package.\nPlease install it first.")

  if (!(type %in% c("width", "mean_dist", "inv_dist", "clust_mean_dist", "clust_width"))) stop("Invalid 'type'. See help file for options.")

  if (dist_method == "auto" && type == "clust_mean_dist")  dist_method <- "haversine"  # otherwise "auto" may use different distance methods for each cluster

  clust_type <- match.arg(clust_type, c("buffer", "hclust"))

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
        message("'CRS' argument ignored, as 'pres.coords' already has one.\n")
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

  if (nrow(pres.coords) > nrow(unique(terra::crds(pres.coords))))
    message("NOTE: Duplicate coordinates found, which can bias distance estimates.\nClean dataset beforehand? (see e.g. ?cleanCoords)\n")

  nrow_in <- nrow(pres.coords)
  pres.coords <- pres.coords[complete.cases(terra::crds(pres.coords)), ]
  nrow_out <- nrow(pres.coords)
  if (verbosity > 0 && nrow_out < nrow_in) message(nrow_in, " input rows; ", nrow_in - nrow_out, " excluded due to missing coordinates; ", nrow_out, " rows used.\n")

  # if (grepl("dist|clust", type)) {
  # length(grep("_dist", type)) > 0   #
  # type %in% c("mean_dist", "inv_dist")  <- should be this one, but wrong result if no CRS
  if (length(grep("_dist", type)) > 0 || clust_type == "hclust") {
    if (is.null(dist_mat)) {
      if (verbosity > 0) message("Computing pairwise distance between points...")

      # if (!terra::is.lonlat(pres.coords)) pres.coords <- terra::project(pres.coords, "EPSG:4326")

      # dist_mat <- geodist::geodist(terra::crds(pres.coords), measure = dist_measure)
      # dist_mat <- terra::distance(pres.coords)
      dist_mat <- distMat(pres.coords, CRS = terra::crs(pres.coords), dist_method = dist_method, verbosity = verbosity)  # 'pres.coords' somehow gets a CRS assigned here...

    } else {
      if (verbosity > 0) message("Using supplied pairwise distance between points...")
    }

    diag(dist_mat) <- dist_mat[lower.tri(dist_mat)] <- NA  # new

    dist_mean <- mean(dist_mat, na.rm = TRUE)
  }  # end if dist


  if (grepl("clust", type)) {

    if (verbosity > 1) message("Computing point clusters...")

    if (clust_type == "hclust") {
      # nrby <- nearby(pres.coords, k = 1)
      # nearest_dist <- distance(pres.coords[nrby[ , 1], ], pres.coords[nrby[ , 2], ], pairwise = TRUE)

      tree <- stats::hclust(d = stats::as.dist(dist_mat), method = "single")
      pres.coords$clust <- stats::cutree(tree, h = clust_dist * 1000)  # km to meters
    }

    if (clust_type == "buffer") {
      buffers <- terra::buffer(pres.coords, width = clust_dist * 1000)  # km to meters
      # clust_polygons <- terra::convHull(pres.coords, by = "clust")
      # buff_radius <- sqrt(terra::expanse(clust_polygons))
      groups <- terra::disagg(terra::aggregate(buffers))
      groups$id <- 1:nrow(groups)
      pres.coords$clust <- terra::extract(groups, pres.coords)$id
    }

    # if (verbosity > 1) message("- got ", length(clusters), " clusters")

    # clust_polygons <- terra::convHull(pres.coords, by = "clust")
    # buff_radius <- sqrt(terra::expanse(clust_polygons))
    clusters <- unique(pres.coords$clust)
    # if (verbosity > 1) message("- got ", length(clusters), " clusters")
  }  # end if clust


  if (type == "mean_dist") {
    reg <- terra::buffer(pres.coords, width = dist_mean * dist_mult)
  }  # end if mean_dist

  else if (type == "inv_dist") {
    # dist_df <- as.data.frame(as.matrix(dist_mat))
    dist_sums <- sapply(dist_mat, sum, na.rm = TRUE)  # sum of distances from each point to all other points
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    dist_sums_01 <- range01(dist_sums)
    dist_sums_01[dist_sums_01 == 0] <- 0.001  # otherwise buffer() error
    reg <- terra::buffer(pres.coords, width = dist_mean * rev(dist_sums_01) * dist_mult)
  }  # end if inv_dist

  else if (type == "clust_mean_dist") {
    if (verbosity > 0) message("Getting pairwise distance within clusters...")  # before loop to avoid message repetitions
    for (i in clusters) {
      # if (verbosity > 1) cat(i, "")
      clust_pts <- pres.coords[pres.coords$clust == i, ]
      # buff_radius <- mean(terra::distance(clust_pts)) * dist_mult
      # buff_radius <- mean(geodist::geodist(terra::crds(clust_pts), measure = dist_measure)) * dist_mult
      # buff_radius <- mean(distMat(clust_pts, CRS = terra::crs(pres.coords), dist_method = dist_method, verbosity = 0), na.rm = TRUE) * dist_mult

      # buff_radius <- mean(terra::distance(clust_pts)) * dist_mult

      # dist_mat_clust <- distMat(clust_pts, CRS = terra::crs(pres.coords), dist_method = dist_method, verbosity = verbosity)
      dist_mat_clust <- dist_mat[pres.coords$clust == i, pres.coords$clust == i, drop = FALSE]

      if (nrow(dist_mat_clust) > 1)  diag(dist_mat_clust) <- dist_mat_clust[lower.tri(dist_mat_clust)] <- NA  # new

      buff_radius <- mean(dist_mat_clust, na.rm = TRUE) * dist_mult

      if (!is.finite(buff_radius) || buff_radius <= 0) buff_radius <- 0.001  # because clusters with only one point get no distance, and a zero-width buffer cannot be computed for points
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

    reg <- terra::buffer(pres.coords, width = "buff_radius")  # uses column value
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

    reg <- terra::buffer(pres.coords, width = "clust_width")  # uses column value
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
                # main = paste("type =", type),
                ...)

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
