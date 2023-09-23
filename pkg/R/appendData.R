
#' Append data
#'
#' @param x object inheriting class 'data.frame' to which to append data.
#' @param y object inheriting class 'data.frame' to append to x, with column names matching those of the corresponding columns in x.
#' @param fill logical, whether to keep in the result all columns of x that are missing in y, filling them with NAs for the rows with no data. TRUE by default. If set to FALSE, the result will keep only the columns also present in y.
#'
#' @return This function returns a data frame with all the columns of x, and the additional rows of y with its values for the columns with matching names in x. If fill is set to FALSE, the result only carries the columns with matching names in both data frames.
#' @export
#'
#' @examples
#' df1 = data.frame(A = 3:1, B = letters[1:3], C = c(1, 0, 1))
#' df2 = data.frame(A = 4:5, B = letters[5:4])
#' appendData(df1, df2)

appendData <- function(x, y, fill = TRUE) {
  # version 1.0 (22 Sep 2023)

  y <- y[ , colnames(y) %in% colnames(x)]
  y2 <- x[1:nrow(y), ]  # y with the structure of x
  y2[] <- NA
  y2[ , colnames(y)] <- y[ , colnames(y)]
  out <- rbind(x, y2)
  if (!fill) out <- out[ , colnames(y)]
  return(out)
}
