fuzzyRangeChange <- function(pred1, pred2, prop = TRUE, na.rm = TRUE, round.digits = 2) {
  
  # version 1.2 (13 Nov 2015)
  
  stopifnot(ncol(pred1) == ncol(pred2),
            all(pred1[!is.na(pred1)] >= 0 && pred1[!is.na(pred1)] <= 1),
            all(pred2[!is.na(pred2)] >= 0 && pred2[!is.na(pred2)] <= 1)
  )
  
  gain <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "expansion", na.rm = na.rm), na.rm = na.rm)
  loss <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "contraction", na.rm = na.rm), na.rm = na.rm)
  stable <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  change <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "change", na.rm = na.rm), na.rm = na.rm)
  
  result <- c(gain, loss, stable, change)
  names(result) <- c("gain", "loss", "stable", "change")
  
  if (prop) {
    n <- length(pred1)
    result <- result / n
  }

  result
}
