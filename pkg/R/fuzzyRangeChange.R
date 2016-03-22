fuzzyRangeChange <- function(pred1, pred2, number = FALSE, prop = TRUE, na.rm = TRUE, round.digits = 2, measures = c("Gain", "Loss", "Stable_presence", "Stable_absence", "Balance"), plot = TRUE, col = colorRampPalette(c("white", "black"))(length(measures)), ...) {
  
  # version 1.4 (22 Mar 2016)
  
  stopifnot(ncol(pred1) == ncol(pred2),
            all(pred1[is.finite(pred1)] >= 0 && pred1[is.finite(pred1)] <= 1),
            all(pred2[is.finite(pred2)] >= 0 && pred2[is.finite(pred2)] <= 1)
  )
  
  values <- vector("numeric", length(measures))
  names(values) <- measures
  if ("Gain" %in% measures)  values["Gain"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "expansion", na.rm = na.rm), na.rm = na.rm)
  if ("Loss" %in% measures)  values["Loss"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "contraction", na.rm = na.rm), na.rm = na.rm)
  if ("Stable_presence" %in% measures)  values["Stable_presence"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  if ("Stable_absence" %in% measures)  values["Stable_absence"] <- sum(fuzzyOverlay(1 - data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  if ("Balance" %in% measures)  values["Balance"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "change", na.rm = na.rm), na.rm = na.rm)
  
  result <- data.frame(Measure = measures, Number = values)
  #rownames(result) <- NULL
  
  if (prop) {
    if (na.rm) n <- length(na.omit(pred1))
    else n <- length(pred1)
    range.size <- sum(pred1, na.rm = na.rm)
    stable.abs <- result[result$Measure == "Stable_absence", "Number"]
    result$Proportion <- result[ , "Number"] / range.size
    result[result$Measure == "Stable_absence", "Proportion"] <- stable.abs / (n - range.size)
  }
  
  if (!number) {
    result <- result[ , -1]
  }
  
  if (plot) {
    barplot(result[ , ncol(result)], legend.text = rownames(result), col = col, ...)
    abline(h = 0)
  }
  
  result[,1] <- NULL
  if (is.finite(round.digits))  result <- round(result, round.digits)

  result
}
