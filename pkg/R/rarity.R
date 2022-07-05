rarity <- function(data, sp.cols = 1:ncol(data), na.rm = TRUE) {
  
  # version 1.1 (5 Jul 2022)
  
  data <- as.data.frame(data)
  
  rarity_single <- function(x) 1 / sum(x, na.rm = na.rm)
  
  if (ncol(data) == 1) return (rarity_single(data))
  if (length(sp.cols) == 1) return (rarity_single(data[ , sp.cols]))
    
  rarity_total <- 0
  
  for (i in sp.cols) {
    rarity_sp <- rarity_single(data[ , i])
    ra <- data[ , i] * rarity_sp
    rarity_total <- rarity_total + ra
  }
  rarity_total
}
