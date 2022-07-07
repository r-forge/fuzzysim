fuzzyRangeChange <- function(pred1, pred2, number = TRUE, prop = TRUE, na.rm = TRUE, round.digits = 2, measures = c("Gain", "Loss", "Stable positive", "Stable negative", "Balance"), plot = TRUE, x.lab = TRUE, ...) #col = colorRampPalette(c("white", "black"))(length(measures)) 
  {
  
  # version 1.8 (7 Jul 2022)
  
  pred1 <- unlist(pred1)
  pred2 <- unlist(pred2)
  
  stopifnot(#ncol(pred1) == ncol(pred2),
            #all(pred1[is.finite(pred1)] >= 0 && pred1[is.finite(pred1)] <= 1),
            #all(pred2[is.finite(pred2)] >= 0 && pred2[is.finite(pred2)] <= 1)
            length(pred1) == length(pred2),
            min(c(pred1, pred2), na.rm = TRUE) >= 0,
            max(c(pred1, pred2), na.rm = TRUE) <= 1
  )

  if (!number & !prop) stop ("Nothing to calculate if both 'number' and 'prop' are FALSE.")
  
  values <- vector("numeric", length(measures))
  names(values) <- measures
  if ("Gain" %in% measures)  values["Gain"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "expansion", na.rm = na.rm), na.rm = na.rm)
  if ("Loss" %in% measures)  values["Loss"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "contraction", na.rm = na.rm), na.rm = na.rm)
  if ("Stable positive" %in% measures)  values["Stable positive"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  if ("Stable negative" %in% measures)  values["Stable negative"] <- sum(fuzzyOverlay(1 - data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  if ("Balance" %in% measures)  values["Balance"] <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "change", na.rm = na.rm), na.rm = na.rm)
  
  result <- data.frame(Measure = measures, Number = values)
  
  if (prop) {
    if (na.rm) n <- length(na.omit(pred1))
    else n <- length(pred1)
    range.size <- sum(pred1, na.rm = na.rm)
    stable.abs <- result[result$Measure == "Stable negative", "Number"]
    result$Proportion <- result[ , "Number"] / range.size
    result[result$Measure == "Stable negative", "Proportion"] <- stable.abs / (n - range.size)
  }
  
  if (!number) {
    result <- result[ , - (ncol(result) - 1)]
  }
  
  if (plot) {
    if (x.lab) xlab <- gsub(x = rownames(result), pattern = " ", replacement = "\n") else xlab <- ""
    barplot(result[ , ncol(result)], names.arg = xlab, ylab = names(result)[ncol(result)], ...)
    abline(h = 0)
  }
  
  result[ , "Measure"] <- NULL
  if (is.finite(round.digits))  result <- round(result, round.digits)

  result
}
