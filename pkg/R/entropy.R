entropy <- function(data, sp.cols = 1:ncol(data), method = "fuzzy", base = exp(1),
                    plot = TRUE, plot.type = "lollipop", na.rm = TRUE, ...) {

  # version 1.2 (12 Jan 2023)

  data_in <- data
  data <- as.data.frame(data)

  rslt <- vector("numeric", length(sp.cols))
  if (!is.vector(data_in))  names(rslt) <- names(data[ , sp.cols])

  if (method == "fuzzy") {
    for (i in 1:length(sp.cols)) {
      values <- data[ , sp.cols[i]]
      complement <- 1 - values
      int <- pmin(values, complement, na.rm = na.rm)
      uni <- pmax(values, complement, na.rm = na.rm)
      rslt[i] <- sum(int, na.rm = na.rm) / sum(uni, na.rm = na.rm)
    }
  }

  else if (method == "Shannon") {
    for (i in 1:length(sp.cols)) {
      values <- data[ , sp.cols[i]]
      if (sum(values, na.rm = na.rm) != 1)  values <- values / sum(values, na.rm = na.rm)
      rslt[i] <- -1 * sum(values * log(values, base = base), na.rm = na.rm)
    }
  }

  else stop ("Invalid 'method'.")

  if (plot == TRUE && ncol(data[ , sp.cols, drop = FALSE]) > 1) {
    plot.type <- match.arg(plot.type, c("lollipop", "barplot"))
    if (plot.type == "lollipop") modEvA::lollipop(rslt, ...)
    else if (plot.type == "barplot") barplot(rslt, ...)
    else message("Invalid 'plot.type'. Plot not produced.")
  }

  return(rslt)
}
