\name{dms2dec}
\alias{dms2dec}
\title{Degree-minute-second to decimal degree coordinates}
\description{
This function converts degree-minute-second geographic coordinates to decimal degree (numeric) coordinates appropriate for mapping and analysis.
}
\usage{
dms2dec(dms,
seps = c("\\\\u00ba", "\\\\u00b0", "\\\\'", "\\\\'", "\\\\\\"", "\\\\\\\\?"))
}
\arguments{
\item{dms}{character vector of geographic coordinates (latitude or longitude) in degree-minute-second-hemisphere format, e.g. 41° 34' 10.956" N (with or without spaces); or in degree-decimal minute format, e.g. 41° 34.1826' N (with or without spaces)}
\item{seps}{character vector of possible separators in 'dms'. The default includes commonly used symbols for degrees, minutes and seconds, converted with stringi::stri_escape_unicode() for portability}
}
\value{
This function returns a numeric vector of the input coordinates after conversion to decimal degree format.
}
\examples{
coords_dms <- structure(list(Longitude = c("31º40'44.12''E", "31º41'23.35''E",
"31º37'01.94''E", "30º53'07.75''E"), Latitude = c("24º54'36.44''S",
"24º05'02.09''S", "25º09'46.72''S", "24º12'09.02''S")), row.names = c(NA, 4L),
class = "data.frame")
coords_dms

lon_dec <- dms2dec(coords_dms$Longitude)
lat_dec <- dms2dec(coords_dms$Latitude)

coords_dec <- sapply(coords_dms, dms2dec)
coords_dec
}
\author{
A. Marcia Barbosa (https://github.com/AMBarbosa) with contributions by Paul Melloy (https://github.com/PaulMelloy)
}
