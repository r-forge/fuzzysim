\name{appendData}
\alias{appendData}
\title{
Append data
}
\description{
This function appends the rows of a dataframe 'y' at the bottom of another dataframe 'x', using the values in the columns with matching names, and filling missing columns with NAs. Columns of 'y' that are not matched in 'x' are not added to the result.
}
\usage{
appendData(x, y, fill = TRUE)
}
\arguments{
  \item{x}{
object inheriting class 'data.frame' to which to append data.
}
  \item{y}{
object inheriting class 'data.frame' to append to \code{x}, with column names matching those of the corresponding columns in \code{x}.
}
  \item{fill}{
logical, whether the result should keep all columns of \code{x} that are missing in \code{y}, filling them with NAs in the rows with no data. The default is TRUE. If set to FALSE, the result will keep only the columns of \code{x} that are also present in \code{y}.
}
}
\value{
This function returns a data frame with all the columns and rows of \code{x}, extended with the rows of \code{y} with its values for the columns with matching names in \code{x}. If \code{fill} is set to FALSE, the result only carries the columns with matching names in both data frames.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{rbindlist} in package \pkg{data.table}; \code{rbind.fill} in package \pkg{plyr}.
}
\examples{
df1 = data.frame(A = 3:1, B = letters[1:3], C = c(1, 0, 1))
df2 = data.frame(A = 4:5, B = letters[5:4])

appendData(df1, df2)

appendData(df1, df2, fill = FALSE)
}
