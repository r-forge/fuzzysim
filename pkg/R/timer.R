timer <- function(start.time) {
  duration <- difftime(Sys.time(), start.time, units = "auto")
  units <- attr(duration, "units")
  duration <- round(abs(as.numeric(duration)), 1)
  message("It took ", duration, " ", units, ".")
  invisible(paste(duration, units))
}
