distPres <- function(data, sp.cols, coord.cols = NULL, id.col = NULL, dist.mat = NULL, CRS = NULL, method = "auto", suffix = "_D", p = 1, inv = TRUE, verbosity = 2) {
  # version 2.2 (5 Jan 2025)

  data <- as.data.frame(data)

  stopifnot(
    as.matrix(data[, sp.cols]) %in% c(NA, 0, 1),
    length(sp.cols) > 0,
    length(sp.cols) <= ncol(data) - length(coord.cols) - length(id.col),
    !is.null(coord.cols) | !is.null(dist.mat),
    is.null(coord.cols) | length(coord.cols) == 2,
    is.null(coord.cols) | coord.cols %in% 1:ncol(data) | coord.cols %in% colnames(data),
    is.numeric(as.matrix(data[, coord.cols])),
    is.null(id.col) | id.col %in% 1:ncol(data) | id.col %in% colnames(data),
    is.null(dist.mat) | nrow(dist.mat) == nrow(data)
  )

  if (is.null(dist.mat)) {
    dist.mat <- distMat(data[ , coord.cols], CRS = CRS, dist_method = method, verbosity = verbosity)
  }

  if (is.numeric(sp.cols)) sp.cols <- names(data)[sp.cols]
  n.obs <- nrow(data)
  n.subjects <- length(sp.cols)
  pres.dist.mat <- matrix(nrow = n.obs, ncol = n.subjects, dimnames = list(rownames(data), sp.cols))

  for (s in sp.cols) {
    pres_rows <- which(data[ , s] == 1)

    if (length(pres_rows) == 0) {
      pres.dist.mat[ , s] <- NA
      next
    }

    pres.dist.mat[ , s] <- apply(dist.mat[ , pres_rows, drop = FALSE], 1, min)
  }

  if (p != 1) pres.dist.mat <- pres.dist.mat ^ p

  if (inv) {
    d <- pres.dist.mat  # short alias for next command
    pres.dist.mat <- (d - min(d, na.rm = TRUE)) / (max(d, na.rm = TRUE) - min(d, na.rm = TRUE))  # normalize to [0,1]
    pres.dist.mat <- 1 - pres.dist.mat
  }

  colnames(pres.dist.mat) <- paste0(colnames(data[ , sp.cols, drop = FALSE]), suffix)

  if (!is.null(id.col)) {
    pres.dist.mat <- data.frame(data[ , id.col, drop = FALSE], pres.dist.mat)
    # if (is.character(id.col)) colnames(pres.dist.mat)[1] <- id.col
    # else colnames(pres.dist.mat)[1] <- colnames(data)[id.col]
  }

  return(pres.dist.mat)
}
