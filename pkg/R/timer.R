timer <- function(..., digits = 1) {

  # version 2.1 (13 Oct 2025)

  expr <- substitute(...)
  env <- parent.frame()

  # Detect if expr is a symbol (e.g. 'start.time') and refers to a timestamp
  if (is.symbol(expr)) {
    val <- eval(expr, env)
    if (inherits(val, "POSIXct") || inherits(val, "POSIXlt")) {
      duration <- as.numeric(difftime(Sys.time(), val, units = "secs"))

    } else {
      duration <- system.time(eval(expr, env))["elapsed"]
    }

  } else {
    # If it's not a symbol, treat it as an expression to time
    duration <- system.time(eval(expr, env))["elapsed"]
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
