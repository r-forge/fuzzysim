timer <- function(start.time) {
  duration <- difftime(Sys.time(), start.time, units = "auto")
  units <- attr(duration, "units")
  duration <- round(abs(as.numeric(duration)), 1)
  message(duration, " ", units, " elapsed.")
  invisible(paste(duration, units))
}
