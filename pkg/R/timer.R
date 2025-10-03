timer <- function(start.time, digits = 1) {

  # version 2.0 (2 Oct 2025)

  if (inherits(start.time, "POSIXct") || inherits(start.time, "POSIXlt")) {
    duration <- difftime(Sys.time(), start.time, units = "secs")
    # units <- attr(duration, "units")
    # duration <- round(abs(as.numeric(duration)), digits)
    duration <- as.numeric(duration)

  } else {  # new, if expression
    expr <- substitute(start.time)
    st <- system.time(eval(expr, parent.frame()))
    duration <- st["elapsed"]
  }

  if (duration < 60) {
    units <- "secs"
  } else if (duration < 3600) {
    duration <- duration / 60
    units <- "mins"
  } else {
    duration <- duration / 3600
    units <- "hours"
  }

  duration <- round(duration, digits)
  message("It took ", duration, " ", units, ".")
  invisible(paste(duration, units))
}
