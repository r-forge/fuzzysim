#' Degree-minute-second to decimal degree coordinates
#'
#' @param dms character vector of geographical coordinates (latitude or longitude) in degree-minute-second-hemisphere format, e.g. 41° 34' 10.956" N (with or without spaces); or in degree-decimal minute format, e.g. 41° 34.1826' N (with or without spaces)
#' @param seps character vector of possible separators in 'dms'. The default includes commonly used symbols for degrees, minutes and seconds, converted with stringi::stri_escape_unicode() for portability
#'
#' @return numeric vector of the input coordinates after conversion to decimal degree format
#' @export
#' @importFrom stringi stri_unescape_unicode
#'
#' @author A. Marcia Barbosa (https://github.com/AMBarbosa) with contributions by Paul Melloy (https://github.com/PaulMelloy)
#' @examples
#' coords_dms <- structure(list(Longitude = c("31º40'44.12''E", "31º41'23.35''E",
#' "31º37'01.94''E", "30º53'07.75''E"), Latitude = c("24º54'36.44''S",
#' "24º05'02.09''S", "25º09'46.72''S", "24º12'09.02''S")), row.names = c(NA, 4L),
#' class = "data.frame")
#' coords_dms
#'
#' lon_dec <- dms2dec(coords_dms$Longitude)
#' lat_dec <- dms2dec(coords_dms$Latitude)
#'
#' coords_dec <- sapply(coords_dms, dms2dec)
#' coords_dec

dms2dec <- function(dms, seps = c("\\u00ba", "\\u00b0", "\\'", "\\'", "\\\"", "\\\\?")) {
  # original seps = c("º", "°", "\'", "’", "\"", "\\?")
  # converted with stringi::stri_escape_unicode() to c("\\u00ba", "\\u00b0", "\\'", "\\'", "\\\"", "\\\\?")
  # then unescaped in function code below

  # version 1.7 (30 May 2024)

  seps <- stringi::stri_unescape_unicode(seps)
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  new_split_string <- "_splitHere_"
  for (s in seps) dms <- gsub(pattern = s, replacement = new_split_string, x = dms, useBytes = FALSE)
  dms <- gsub(paste0(new_split_string, new_split_string), new_split_string, dms, useBytes = TRUE)

  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)

  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]

    # check if degrees decimal minutes (Paul Melloy contrib.)
    dm <- unlist(strsplit(min[i], "[.]"))
    if (length(dm) > 1) {
      if (length(dm) > 2)
        stop("Input minutes format is not recongnisable")

      sec[i] <- as.numeric(paste0("0.", dm[2], collapse = "")) * 60
      min[i] <- dm[1]
      if(min[i] > 60) stop("minutes entry exceeds 60, check minutes input")

      hem[i] <-
        splits[[i]][splits[[i]] %in% c("N", "S", "E", "W", "n", "s", "e", "w")]

    } else {
      if (length(splits[[i]]) < 4) {
        hem[i] <- splits[[i]][3]
      } else {
        sec[i] <- splits[[i]][3]
        hem[i] <- splits[[i]][4]
      }
    }
  }

  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified in position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}  # end dms2dec function
