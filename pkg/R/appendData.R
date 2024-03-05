
#' Append data
#'
#' @param data1 object inheriting class 'data.frame' to which to append data.
#' @param data2 object inheriting class 'data.frame' to append to data1, with column names matching those of the corresponding columns in data1.
#' @param fill logical, whether to keep in the result all columns of data1 that are missing in data2, filling them with NAs for the rows with no data. TRUE by default. If set to FALSE, the result will keep only the columns also present in data2.
#'
#' @return This function returns a data frame with all the columns of data1, and the additional rows of data2 with its values for the columns with matching names in data1. If fill is set to FALSE, the result only carries the columns with matching names in both data frames.
#' @export
#'
#' @examples
#' df1 = data.frame(A = 3:1, B = letters[1:3], C = c(1, 0, 1))
#' df2 = data.frame(A = 4:5, B = letters[5:4])
#' appendData(df1, df2)

appendData <- function(data1, data2, fill = TRUE, add.source = TRUE) {
  # version 1.3 (25 Jan 2024)

  data1 <- as.data.frame(data1)
  data2 <- as.data.frame(data2)

  column_match <- colnames(data2) %in% colnames(data1)
  if (sum(column_match) == 0) warning("No matching column names, so only NAs were added.")
  data2 <- data2[ , column_match]

  # data2.2 <- data1[1:nrow(data2), ]  # data2 with the structure of data1
  # data2.2[] <- NA
  data2.2 <- data.frame(matrix(nrow = nrow(data2), ncol = ncol(data1)))  # data2 with all columns of data1; tried adding dimnames here, but error afterwards
  colnames(data2.2) <- colnames(data1)

  data2.2[ , colnames(data2)] <- data2[ , colnames(data2)]  # fill in values where available
  out <- rbind(data1, data2.2)

  if (add.source) {
    # out$add.source <- substitute(data2)
    # out$add.source[1:nrow(data1)] <- substitute(data1)
    # out$add.source <- deparse(substitute(data2))
    # out$add.source[1:nrow(data1)] <- deparse(substitute(data1))
    # Error in `$<-.data.frame`(`*tmp*`, "add.source", value = c("structure(list(species = c(\"Daboia_mauritanica\", \"Daboia_mauritanica\", ", : replacement has 108 rows, data has 198
    out$appendSource <- deparse(quote(data2))
    out$appendSource[1:nrow(data1)] <- deparse(quote(data1))
  }

  if (!fill) {
    if (add.source)  out <- out[ , c(colnames(data2), "appendSource")]
    else out <- out[ , colnames(data2)]
  }

  return(out)
}
