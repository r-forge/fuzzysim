\name{appendData}
\alias{appendData}
\title{
Append data
}
\description{
This function appends the rows of a dataframe 'data2' at the bottom of another dataframe 'data1', using the values in the columns with matching names, and (optionally, by default) filling missing columns with NAs.
}
\usage{
appendData(data1, data2, fill = TRUE, add.source = TRUE)
}
\arguments{
  \item{data1}{
object inheriting class 'data.frame' (or that can be coerced with 'as.data.frame') to which to append data.
}
  \item{data2}{
object inheriting class 'data.frame' (or that can be coerced with 'as.data.frame') to append to 'data1', with column names matching those of the corresponding columns in 'data1'. Both datasets can have more columns than those whose names match.
}
  \item{fill}{
logical, whether the result should keep all columns of 'data1' that are missing in 'data2', filling them with NAs in the rows with no data. The default is TRUE. If set to FALSE, the result will keep only the columns of 'data1' that are also present in 'data2'.
}
  \item{add.source}{
logical, whether the result should include an additional column saying from which input data frame ('data1' or 'data2') each row came.
}
}
\details{
This function is asymmetric, i.e. \code{appendData(data1, data2)} may output different columns than \code{appendData(data2, data1)}. 'data1' dictates the columns that the result will have. Columns of 'data2' that are not matched in 'data1' are not kept in the output.
}
\value{
This function returns a data frame with all the columns and rows of 'data1', extended with the rows of 'data2' with its values for the columns with matching names in 'data1'. By default, with 'add.source = TRUE', there is also an additional column specifying the source input object. If 'fill' is set to FALSE, the result only carries the columns with matching names in both data frames.
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

appendData(df1, df2, fill = FALSE, add.source = FALSE)
}
