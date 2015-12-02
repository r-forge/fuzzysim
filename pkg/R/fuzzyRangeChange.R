fuzzyRangeChange <- function(pred1, pred2, prop = TRUE, na.rm = TRUE, round.digits = 2) {
  
  # version 1.3 (23 Nov 2015)
  
  stopifnot(ncol(pred1) == ncol(pred2),
            all(pred1[is.finite(pred1)] >= 0 && pred1[is.finite(pred1)] <= 1),
            all(pred2[is.finite(pred2)] >= 0 && pred2[is.finite(pred2)] <= 1)
  )
  
  gain <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "expansion", na.rm = na.rm), na.rm = na.rm)
  loss <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "contraction", na.rm = na.rm), na.rm = na.rm)
  stable.pres <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  stable.abs <- sum(fuzzyOverlay(1 - data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  balance <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "change", na.rm = na.rm), na.rm = na.rm)
  
  result <- data.frame(Measure = c("Gain", "Loss", "Stable_presence", "Stable_absence", "Balance"), Number = c(gain, abs(loss), stable.pres, stable.abs, balance))
  
  if (prop) {
    if (na.rm) n <- length(na.omit(pred1))
    else n <- length(pred1)
    range.size <- sum(pred1, na.rm = na.rm)
    stable.abs <- result[result$Measure == "Stable_absence", "Number"]
    result[ , "Number"] <- result[ , "Number"] / range.size
    result[result$Measure == "Stable_absence", "Number"] <- stable.abs / (n - range.size)
    names(result)[2] <- "Proportion"
  }

  result
}
