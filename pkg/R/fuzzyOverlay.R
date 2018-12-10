fuzzyOverlay <- function(data, 
                         overlay.cols = 1:ncol(data),
                         op = "intersection",
                         na.rm = FALSE,
                         round.digits = 2
                         ) {
  
  # version 1.4 (10 Dec 2018)
  
  data <- data[ , overlay.cols]
  #stopifnot(all(data[!is.na(data), ] >= 0 && data[!is.na(data), ] <= 1))
  #stopifnot(all(data[!is.na(data)] >= 0 && data[!is.na(data)] <= 1))
  stopifnot(min(data, na.rm = TRUE) >= 0,
            max(data, na.rm = TRUE) <= 1)
  
  if (op == "consensus") rowSums(data, na.rm = na.rm) / ncol(data)
  else if (op %in% c("fuzzy_and", "intersection")) apply(data, MARGIN = 1, FUN = min, na.rm = na.rm)
  else if (op == "prob_and") apply(data, MARGIN = 1, FUN = prod, na.rm = na.rm)
  else if (op %in% c("fuzzy_or", "union")) apply(data, MARGIN = 1, FUN = max, na.rm = na.rm)
  else if (op == "prob_or") apply(data, MARGIN = 1, FUN = sum, na.rm = na.rm) - apply(data, MARGIN = 1, FUN = prod, na.rm = na.rm)
  else if (op == "maintenance") ifelse(round(data[,2], digits = round.digits) == round(data[,1], digits = round.digits), round(data[,1], digits = round.digits), 0)
  
  else if (op %in% c("xor", "AnotB", "expansion", "contraction", "change")) {
    if (ncol(data) != 2) stop ("This 'op' works only for 'data' with 2 columns.")
    if (op == "xor") pmax(pmin(data[,1], 1 - data[,2], na.rm = na.rm), pmin(1 - data[,1], data[,2], na.rm = na.rm), na.rm = na.rm)  # http://www.dmitry-kazakov.de/ada/fuzzy.htm#Fuzzy
    else if (op == "AnotB") pmin(data[,1], 1 - data[,2], na.rm = na.rm)
    else if (op == "expansion") ifelse(data[,2] > data[,1], 
                                       data[,2] - data[,1], 
                                       0)
    else if (op == "contraction") ifelse(data[,2] < data[,1], 
                                         data[,2] - data[,1], 
                                         0)
    else if (op == "change") data[,2] - data[,1]
  }
  else stop ("Invalid 'op' name.")
}
