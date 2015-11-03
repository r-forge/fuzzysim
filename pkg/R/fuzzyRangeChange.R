fuzzyRangeChange <- function(pred1, pred2, na.rm = FALSE, round.digits = 2) {
  
  stopifnot(ncol(pred1) == ncol(pred2),
            all(pred1[!is.na(pred1)] >= 0 && pred1[!is.na(pred1)] <= 1),
            all(pred2[!is.na(pred2)] >= 0 && pred2[!is.na(pred2)] <= 1)
  )
  n <- length(pred1)
  
  gain <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "expansion", na.rm = na.rm), na.rm = na.rm)
  prop_gain <- gain / n
  loss <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "contraction", na.rm = na.rm), na.rm = na.rm)
  prop_loss <- loss / n
  stable <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "maintenance", na.rm = na.rm), na.rm = na.rm)
  prop_stable <- stable / n
  total_change <- sum(fuzzyOverlay(data.frame(pred1, pred2), op = "change", na.rm = na.rm), na.rm = na.rm)
  prop_change <- total_change / n
  
  list(gain = gain, prop_gain = prop_gain, loss = loss, prop_loss = prop_loss, stable = stable, prop_stable = prop_stable, total_change = total_change, prop_change = prop_change)
}