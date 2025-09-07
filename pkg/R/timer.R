timer <- function(start.time) {
  duration <- difftime(Sys.time(), start.time, units = "auto")
  units <- attr(duration, "units")
  duration <- round(abs(as.numeric(duration)), 1)
  message("It took ", duration, " ", units, ".")
  invisible(paste(duration, units))
}

# timer <- function(expr) {
#   st <- system.time(expr)
#   mins <- st["elapsed"] / 60
#   message(paste("Took", round(mins, 1), "minutes."))
# }
