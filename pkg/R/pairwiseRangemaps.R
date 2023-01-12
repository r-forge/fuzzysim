pairwiseRangemaps <- function(rangemaps,
                              projection = NULL,
                              diag = TRUE,
                              unions = TRUE,
                              verbosity = 2,
                              Ncpu = 1,  # parallel::parSapply if larger
                              nchunks = 1,  # nr chunks to split results
                              subchunks = NULL,  # chunks for partial pwrm
                              filename = "rangemap_matrix.csv"
) {
  # version 2.0 (12 Jan 2023)

#  if (!requireNamespace("PBSmapping", quietly = TRUE)) {
#    stop("This function needs R package 'PBSmapping' - please install it first.", call. = FALSE)
#  }

#   requireNamespace("tools")
#   requireNamespace("PBSmapping")
#   requireNamespace("maptools")
#   requireNamespace("sp")
# #  requireNamespace("rgeos")

  # required_pkgs <- c("tools", "PBSmapping", "maptools", "sp")
  # missing_pkgs <- required_pkgs[!(required_pkgs %in% .packages())]
  # if (length(missing_pkgs) > 0) stop("The following packages need to be loaded first: ", paste(missing_pkgs, collapse = ", "))

  if (!is.null(projection)) message("'projection' argument now ignored; see help file.")

  stopifnot(nchunks > 0 || nchunks == "decreasing")

  if (!is.null(filename) && file.exists(filename)) stop ("filename '", filename, "' already exists in the working directory; please choose another 'filename' or (re)move/rename the existing matrix.")

  if (nchunks != 1) {
    chunks.folder <- "R_chunks_IN-PROGRESS"
    chunks.folder.finish <- "R_chunks_FINISH"
    if (file.exists(chunks.folder)) stop ("Another 'R_chunks_IN-PROGRESS' folder currently exists in the working directory; please (re)move it or rename it first.")
    if (file.exists(chunks.folder.finish)) stop ("Another 'R_chunks_FINISH' folder currently exists in the working directory; please (re)move it or rename it first.")
    dir.create(chunks.folder)
    #on.exit(unlink(chunks.folder, recursive = TRUE))
    on.exit(file.rename(chunks.folder, chunks.folder.finish))
  }

  if (Ncpu > 1) {
    requireNamespace("parallel")
    # if ("parallel" %in% .packages()) stop("for Ncpu > 1, package 'parallel' needs to be loaded first.")
    if (Ncpu > parallel::detectCores()) {
      Ncpu <- parallel::detectCores()
      message("\nNOTE: 'Ncpu' reduced to ", Ncpu, " to match the existing cores in this machine.")
    }
    cluster <- parallel::makeCluster(Ncpu)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
  }  # end if Ncpu>1

  n.rangemaps <- length(rangemaps)
  # rangemap.names <- basename(tools::file_path_sans_ext(rangemaps))  # for 'PBSmapping'
  rangemap.names <- basename(rangemaps)  # for 'terra'
  rangemap.names <- gsub(pattern = " ", replacement = "_", x = rangemap.names)
  if (verbosity > 0) {
    message(n.rangemaps, " range maps to intersect. You may need some patience!")
    if (verbosity > 1) message("\nRange map names: \n", paste(rangemap.names, collapse = "\n"))
  }

  if (is.numeric(nchunks) && nchunks > n.rangemaps) stop("'nchunks' cannot exceed the number of rangemaps.")

  start.time <- Sys.time()
  message("\nSTARTED: ", start.time)
  on.exit(timer(start.time), add = TRUE)

  if (verbosity > 0) message("\nImporting rangemaps...")  #  to PBSmapping format
  rangemap.list <- vector("list", n.rangemaps)
  names(rangemap.list) <- rangemap.names
  for (m in 1:n.rangemaps) {
    # rangemap.list[[m]] <- assign(rangemap.names[m], PBSmapping::importShapefile(rangemaps[m], projection = projection))
    rangemap.list[[m]] <- assign(rangemap.names[m], terra::makeValid(terra::vect(rangemaps[m])))
    rm(list = ls(pattern = rangemap.names[m]))  # removes loose rangemap from wkspace
  }; rm(m)
  gc()

  n.pairs <- length(combn(n.rangemaps, m = 2)) / 2
  if (verbosity > 0)  message("\nCalculating ", n.pairs, " pairwise intersections...\n(note that computation time varies with range map size)")

  intArea <- function(x, y) {  # x, y are polygons
    # int.pol <- PBSmapping::joinPolys(x, y, operation = "INT")
    int.pol <- terra::crop(x, y)
    if (is.null(int.pol)) return(0)
    # sum(PBSmapping::calcArea(int.pol)[ , "area"])
    sum(terra::expanse(int.pol))
  }  # end 'intArea' function

  lowerTriangInt <- function(rangemap.name, rangemap.list) {
    n.rangemaps <- length(rangemap.list)
    list.names <- names(rangemap.list)
    if (is.null(list.names) || !(rangemap.name %in% list.names)) stop("rangemap.name must be among names(rangemap.list)")
    last.intersect <- which(list.names == rangemap.name) - 1
    if (last.intersect == 0) return(rep(NA, n.rangemaps))
    intersect.sublist <- rangemap.list[1:last.intersect]
    if (Ncpu > 1) ints <- parallel::parSapply(cluster, intersect.sublist, intArea, y = rangemap.list[[rangemap.name]])
    else ints <- sapply(intersect.sublist, intArea, y = rangemap.list[[rangemap.name]])
    c(ints, rep(NA, n.rangemaps - length(ints)))  # fill rest of matrix row
  }  # end 'lowerTriangInt' function

  if (nchunks == 1) {
    rangemap.matrix <- t(sapply(rangemap.names, lowerTriangInt, rangemap.list = rangemap.list))  # 't' because sapply works on columns and turns matrix around
    # colnames(rangemap.matrix) <- rownames(rangemap.matrix) <- rangemap.names
    colnames(rangemap.matrix) <- rownames(rangemap.matrix) <- tools::file_path_sans_ext(rangemap.names)
  }

  else {  # if nchunks != 1

    if (is.numeric(nchunks)) {
      nchunks <- round(nchunks)
      message("\n[Splitting intersections matrix into ", nchunks, " chunks of rows. Intermediate results will be saved as 'intersections_chunkX.csv' files in a temporary folder called '", chunks.folder, "' in the working directory - LEAVE IT THERE until this function has finished running!]\n")
      chunks <- split(rangemap.names, cut(seq_along(rangemap.names), nchunks, labels = FALSE))  # http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r, answer by mathheadinclouds
    }  # end if numeric chunks

    else if (nchunks == "decreasing") {

      growingChunks <- function(n, decreasing = TRUE) {
        f <- r <- 1
        while(length(r) <= n) {
          f <- f + 1
          r <- c(r, rep(f, each = f))
        }
        if (decreasing) r <- rev(r)
        r[1:n]
      }  # end growingChunks function

      fac <- growingChunks(n.rangemaps)
      n.chunks <- length(unique(fac))
      chunks <- rev(split(rangemap.names, f = fac))
      message("\n[Splitting intersections matrix into ", n.chunks, " chunks with decreasing number of rows. Intermediate results will be saved as 'intersections_chunkX.csv' files in a temporary folder called '", chunks.folder, "' in the working directory - LEAVE IT THERE until this function has finished running!]\n")
    } else stop ("Invalid 'nchunks' value.")

    #if (!is.null(subchunks)) chunks <- chunks[subchunks]
    if (is.null(subchunks)) subchunks <- 1:length(chunks)

    for (ch in 1:length(chunks)) {
      if (!(ch %in% subchunks)) next
      chunk.time <- Sys.time()
      message("Computing chunk ", ch, " (started ", chunk.time, ")...", sep = "")
      chunk.rangemap.names <- rangemap.names[rangemap.names %in% chunks[[ch]]]
      intersections <- t(sapply(chunk.rangemap.names, lowerTriangInt, rangemap.list = rangemap.list))
      # colnames(intersections) <- rangemap.names  # with 'PBSmapping'
      # rownames(intersections) <- chunk.rangemap.names  # with 'PBSmapping'
      colnames(intersections) <- tools::file_path_sans_ext(rangemap.names)  # with 'terra'
      rownames(intersections) <- tools::file_path_sans_ext(chunk.rangemap.names)  # with 'terra'

      write.csv(intersections, file = paste0(chunks.folder, "/intersections_chunk", ch, ".csv"), row.names = TRUE)
      rm(intersections)
      timer(chunk.time)
      gc()
    }  # end for ch

    if (verbosity > 0) message("\nAssembling intersections matrix...")
    chunk.files <- list.files(chunks.folder, pattern = "*.csv", full.names = TRUE)
    rangemap.matrix <- do.call(rbind, lapply(chunk.files, read.csv, row.names = 1))
    rangemap.matrix <- as.matrix(rangemap.matrix)
  } # end else (if chunks != 1)

  if (diag) {
    if (verbosity > 0) message("\nCalculating individual range map areas (matrix diagonal)...")
    #for (m in subchunks[1]:n.rangemaps) for (n in 1:n.rangemaps) {
    for (m in 1:n.rangemaps) {
      map <- rangemap.list[[rangemap.names[m]]] #get(rangemap.names[m])
      # rangemap.matrix[m, m] <- sum(PBSmapping::calcArea(map)[ , "area"])
      rangemap.matrix[m, m] <- sum(terra::expanse(map))
    }  # end for m
  }  # end if diag

  if (unions) {
    if (verbosity > 0) message("\nCalculating pairwise unions...")

    uniArea <- function(inds, mat) {  # matrix indices
      r <- inds[1]
      c <- inds[2]
      area1 <- mat[r, r]
      area2 <- mat[c, c]
      int <- mat[c, r]
      area1 + area2 - int
    }  # end 'uniArea' function

    upper.inds <- triMatInd(rangemap.matrix, lower = FALSE, list = TRUE)
    unions <- sapply(upper.inds, uniArea, mat = rangemap.matrix)
    #rangemap.matrix[upper.tri(rangemap.matrix)] <- unions  # this would fill upper diagonal by column, I need it by row
    for (i in 1:length(upper.inds)) {
      rangemap.matrix[upper.inds[[i]][1], upper.inds[[i]][2]] <- unions[i]
    }

  }  # end if unions

  if (!is.null(filename)) {
    write.csv(rangemap.matrix, filename)
    message("Results saved also as ", filename, " in the working directory.")
  }

  message("\nFINISHED: ", Sys.time())
  rangemap.matrix

}  # end pairwiseRangemaps function
