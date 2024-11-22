fuzzyOverlay <- function(data,
                         overlay.cols = NULL,
                         op = "intersection",
                         na.rm = FALSE,
                         round.digits = 2
) {

  # version 2.1 (22 Nov 2024)

  data_in <- data

  if (inherits(data_in, "SpatRaster")) {
    data <- terra::values(data, na.rm = FALSE)
  } else {
    data <- as.data.frame(data)  # converts vector, matrix, tibble
  }

  if (!is.null(overlay.cols))
    data <- data[ , overlay.cols]


  #stopifnot(all(data[!is.na(data), ] >= 0 && data[!is.na(data), ] <= 1))
  #stopifnot(all(data[!is.na(data)] >= 0 && data[!is.na(data)] <= 1))
  stopifnot(min(data, na.rm = TRUE) >= 0,
            max(data, na.rm = TRUE) <= 1)


  if (op == "consensus")
    out <- rowSums(data, na.rm = na.rm) / ncol(data)

  else if (op %in% c("fuzzy_and", "intersection"))
    out <- apply(data, MARGIN = 1, FUN = min, na.rm = na.rm)

  else if (op == "prob_and")
    out <- apply(data, MARGIN = 1, FUN = prod, na.rm = na.rm)

  else if (op %in% c("fuzzy_or", "union"))
    out <- apply(data, MARGIN = 1, FUN = max, na.rm = na.rm)

  else if (op == "prob_or")
    out <- apply(data, MARGIN = 1, FUN = sum, na.rm = na.rm) - apply(data, MARGIN = 1, FUN = prod, na.rm = na.rm)

  else if (op == "maintenance")
    out <- ifelse(round(data[,2], digits = round.digits) == round(data[,1], digits = round.digits),
                  round(data[,1], digits = round.digits),
                  0)

  else if (op %in% c("xor", "AnotB", "expansion", "contraction", "change")) {
    if (ncol(data) != 2)
      stop ("This 'op' works only for 'data' with 2 columns or layers.")

    if (op == "xor")
      out <- pmax(pmin(data[,1], 1 - data[,2], na.rm = na.rm),
                  pmin(1 - data[,1], data[,2], na.rm = na.rm),
                  na.rm = na.rm)  # http://www.dmitry-kazakov.de/ada/fuzzy.htm#Fuzzy

    else if (op == "AnotB")
      out <- pmin(data[,1], 1 - data[,2], na.rm = na.rm)

    else if (op == "expansion")
      out <- ifelse(data[,2] > data[,1],
                    data[,2] - data[,1],
                    0)

    else if (op == "contraction")
      out <- ifelse(data[,2] < data[,1],
                    data[,2] - data[,1],
                    0)

    else if (op == "change")
      out <- data[,2] - data[,1]
  }

  else stop ("Invalid 'op' name.")


  if (inherits(data_in, "SpatRaster")) {
    out_rast <- terra::rast(data_in[[1]], vals = NA)
    # terra::set.values(out_rast, cells = terra::cells(out_rast), values = out)
    out_rast[] <- out
    return(out_rast)

  } else return(out)
}

