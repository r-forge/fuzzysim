sharedFav <- function(strong_F, weak_F, conf = 0.95, main = "Shared favourability") {
  stopifnot(length(strong_F) == length(weak_F))
  opar <- par(no.readonly = T)
  par(mar = c(4, 4, 2, 4.5))
  
  F_intersection <- fuzzyOverlay(cbind(strong_F, weak_F), op = "intersection")
  F_union <- fuzzyOverlay(cbind(strong_F, weak_F), op = "union")
  Fovl <- sum(F_intersection, na.rm = TRUE) / sum(F_union, na.rm = TRUE)
  
  brks <- seq(0, 1, by = 0.1)
  bins <- 1:10
  bin <- cut(F_intersection, breaks = brks, labels = bins)
  
  strong_mean <- tapply(strong_F, INDEX = bin, FUN = mean)
  weak_mean <- tapply(weak_F, INDEX = bin, FUN = mean)
  strong_ci <- tapply(strong_F, INDEX = bin, FUN = function(x) t.test(x, conf.level = conf, na.action = na.pass)$conf.int)
  weak_ci <- tapply(weak_F, INDEX = bin, FUN = function(x) t.test(x, conf.level = conf)$conf.int)
  strong_ci[names(Filter(is.null, strong_ci))] <- NA
  weak_ci[names(Filter(is.null, weak_ci))] <- NA
  strong_ci_up <- unlist(lapply(strong_ci, `[`, 2))
  strong_ci_dn <- unlist(lapply(strong_ci, `[`, 1))
  weak_ci_up <- unlist(lapply(weak_ci, `[`, 2))
  weak_ci_dn <- unlist(lapply(weak_ci, `[`, 1))
  
  bin_size <- table(bin)
  names(bin_size) <- names(bins)
  props <- bin_size / length(bin)
  bin <- as.integer(bin)
  
  bar_plot <- barplot(rep(NA, length(bins)), ylim = c(0, 1), xlab = "Favourability intersection", ylab = "Mean favourability", names.arg = brks[-1], main = main)
  col_bar <- "grey50"
  col_ci <- "grey"
  poly_left <- mean(bar_plot[2:3])
  poly_right <- mean(bar_plot[8:9])
  polygon(x = c(poly_left, poly_left, poly_right, poly_right), y = c(0, 1, 1, 0), col = "lightgrey", border = NA, density = 25, angle = -45)
  par(new = TRUE)
  barplot(props, col = col_bar, border = FALSE, xaxt = "n", yaxt = "n", add = TRUE)
  axis(side = 4, col = col_bar, col.axis = col_bar, col.ticks = col_bar, col.lab = col_bar)
  mtext(side = 4, line = 3, "Proportion of localities", col = col_bar)
  abline(h = 0.8, col = "grey", lty = 3)
  
  strong_x <- bar_plot - 0.1
  weak_x <- bar_plot + 0.1
  arrows(strong_x, strong_ci_dn, strong_x, strong_ci_up, code = 3, length = 0.03, angle = 90, col = col_ci)
  arrows(weak_x, weak_ci_dn, weak_x, weak_ci_up, code = 3, length = 0.03, angle = 90, col = col_ci)
  lines(x = strong_x, y = strong_mean, lwd = 2, lty = 1)
  lines(x = weak_x, y = weak_mean, lwd = 2, lty = 2)
  points(x = strong_x, y = strong_mean, pch = 20)
  points(x = weak_x, y = weak_mean, pch = 15, cex = 0.8)
  
  #sharedF <- rep(2, length(strong_F))
  #sharedF[bin <= 2] <- 0
  #sharedF[bin > 8] <- 1
  #sharedF[strong_F >= 0.8 & weak_F < 0.8 & weak_F >= 0.2] <- 3
  
  par(opar)
  return(Fovl)
}
