sharedFav <- function(strong_F, weak_F, conf = 0.95, bin_interval = "0.1", ...) {
  
  # version 1.3 (2 May 2022)
  
  strong_F <- unlist(strong_F)
  weak_F <- unlist(weak_F)

  stopifnot(length(strong_F) == length(weak_F),
            bin_interval %in% c("0.1", "quantiles"))
  
  opar <- par(no.readonly = TRUE)
  par(mar = c(4, 4, 2, 4.5))
  on.exit(par(opar))
  
  F_int <- fuzzyOverlay(cbind(strong_F, weak_F), op = "intersection")
  F_uni <- fuzzyOverlay(cbind(strong_F, weak_F), op = "union")
  F_ovl <- sum(F_int, na.rm = TRUE) / sum(F_uni, na.rm = TRUE)
  
  bins <- 1:10
  if (bin_interval == "0.1")  brks <- seq(0, 1, by = 0.1)
  else if (bin_interval == "quantiles")  brks <- quantile(F_int, seq(0, 1, 0.1))
  bin <- cut(F_int, breaks = brks, labels = bins)
  
  bin_size <- table(bin)
  #names(bin_size) <- names(bins)
  props <- bin_size / length(bin)
  #bin <- as.integer(bin)
  
  # plot bin props:
  bar_plot <- barplot(rep(NA, length(bins)), ylim = c(0, 1), xlab = "Favourability intersection", ylab = "Mean favourability", names.arg = paste0(round(brks[-1], 2), "]"), ...)
  col_bar <- "grey50"
  col_ax <- "grey40"
  poly_left <- mean(bar_plot[2:3])
  poly_right <- mean(bar_plot[8:9])
  polygon(x = c(poly_left, poly_left, poly_right, poly_right), y = c(0, 1, 1, 0), col = "lightgrey", border = NA, density = 25, angle = -45)
  par(new = TRUE)
  barplot(props, col = col_bar, border = FALSE, xaxt = "n", yaxt = "n", add = TRUE)
  axis(side = 4, col = col_ax, col.axis = col_ax, col.ticks = col_ax, col.lab = col_ax)
  mtext(side = 4, line = 3, "Proportion of localities", col = col_ax)
  abline(h = 0.8, col = "grey", lty = 3)
  
  # compute fav means:
  strong_mean <- tapply(strong_F, INDEX = bin, FUN = mean)
  weak_mean <- tapply(weak_F, INDEX = bin, FUN = mean)
  
  # add fav means to plot:
  strong_x <- bar_plot - 0.1  # shift so that CIs don't overlap
  weak_x <- bar_plot + 0.1
  lines(x = strong_x, y = strong_mean, lwd = 2, lty = 1)
  lines(x = weak_x, y = weak_mean, lwd = 2, lty = 2)
  points(x = strong_x, y = strong_mean, pch = 20)
  points(x = weak_x, y = weak_mean, pch = 15, cex = 0.8)
  
  # compute confidence intervals:
  if (is.finite(conf)) {
    strong_ci <- tapply(strong_F, INDEX = bin, FUN = function(x) t.test(x, conf.level = conf, na.action = na.pass)$conf.int)
    weak_ci <- tapply(weak_F, INDEX = bin, FUN = function(x) t.test(x, conf.level = conf)$conf.int)
    strong_ci[names(Filter(is.null, strong_ci))] <- NA
    weak_ci[names(Filter(is.null, weak_ci))] <- NA
    strong_ci_up <- unlist(lapply(strong_ci, `[`, 2))
    strong_ci_dn <- unlist(lapply(strong_ci, `[`, 1))
    weak_ci_up <- unlist(lapply(weak_ci, `[`, 2))
    weak_ci_dn <- unlist(lapply(weak_ci, `[`, 1))
    
    # add confidence intervals to plot:
    arrows(strong_x, strong_ci_dn, strong_x, strong_ci_up, code = 3, length = 0.03, angle = 90, col = "grey")
    arrows(weak_x, weak_ci_dn, weak_x, weak_ci_up, code = 3, length = 0.03, angle = 90, col = "grey")
  }  # end if conf
  
  #sharedF <- rep(2, length(strong_F))
  #sharedF[bin <= 2] <- 0
  #sharedF[bin > 8] <- 1
  #sharedF[strong_F >= 0.8 & weak_F < 0.8 & weak_F >= 0.2] <- 3
  
  bins_table <- cbind(bin = bins, bin_max = brks[-1], bin_size = bin_size, bin_prop = props, Fmean_strong = strong_mean, Fmean_weak = weak_mean)
  rownames(bins_table) <- NULL
  bins_table <- as.data.frame(bins_table)
  
  return(list(FOvI = F_ovl, bins_table = bins_table))
}
