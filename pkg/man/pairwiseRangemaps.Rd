\name{pairwiseRangemaps}
\alias{pairwiseRangemaps}
\title{
Pairwise intersection (and union) of range maps
}
\description{
This function takes a set of rangemaps and returns a matrix containing the areas of their pairwise intersections; optionally, also their individual areas and/our their areas of pairwise unions.
}
\usage{
pairwiseRangemaps(rangemaps, projection, diag = TRUE, unions = TRUE, 
verbosity = 2, Ncpu = 1, nchunks = 1, subchunks = NULL, 
filename = "rangemap_matrix.csv")
}
\arguments{
  \item{rangemaps}{
a character vector of rangemap filenames, including folder paths if not in the woorking directory. ESRI shapefile (.shp) is currently the only accepted format. Specifying the extension is optional.
}
  \item{projection}{
argument to be passed to function \code{importShapefile} of package \pkg{PBSmapping}
}
  \item{diag}{
logical, whether to fill the diagonal of the resulting matrix with the area of each rangemap. The default is TRUE, and it is also automatically set to TRUE (as it is necessary) if \code{unions = TRUE}.
}
  \item{unions}{
logical, whether to fill the upper triangle of the resulting matrix with the area of union of each pair of rangemaps. The default is TRUE. It is not as computationally intensive as the intersection, as it is calculated not with spatial but with algebraic operations within the matrix (union = area1 + area2 - intersection).
}
  \item{verbosity}{
integer number indicating the amount of progress messages to display.
}
  \item{Ncpu}{
integer indicating the number of CPUs (central processing units) to employ if parallel computing is to be used. The default is 1 CPU, which implies no parallel computing, but you may want to increase this if you have many and/or large rangemaps and your machine has more cores that can be used simultaneously. You can find out the total number of cores in you machine with the \code{detectCores} function of package \pkg{parallel}; a usually wise option is to use all cores except one (i.e., \code{Ncpu = parallel::detectCores()-1}).
}
  \item{nchunks}{
either an integer indicating the number of chunks of rows in which to divide the results matrix for calculations, or character "decreasing" to indicate that the matrix should be divided into chunks of decreasing number of rows (as intersections are calculated in the lower triangle, rows further down the matrix have an increasing number of intersections to compute). Note, however, that rangemap size, not rangemap number, is the main determinant of computation time. The default is 1 (no division of the matrix) but, if you have many rangemaps, the process can get clogged. With chunks, each set of rows of the matrix is calculated and saved to disk, and the memory is cleaned before the next chunk begins.
}
  \item{subchunks}{
  optional integer vector specifying which chunks to actually calculate. This is useful if a previous, time-consuming run of pairwiseRangemaps was interrupted (e.g. by a power outage) and you want to calculate only the remaining chunks.
}
  \item{filename}{
optional character vector indicating the name of the file to save the resulting matrix to.
}
}
\details{
This calculation can be very intensive and slow, especially if you have many and/or large rangemaps, due to the time needed for spatial operations between maps. You can set nchunks = "decreasing" for the matrix to be calculated in parts and the memory cleaned between one part and the next; and, if your computer has more than one core that you can use, you can increase \code{Ncpu} to get parallel computing.
}
\value{
This function returns a square matrix containing, in the lower triangle, the area of the pair-wise intersections among the input \code{rangemaps}; in the diagonal (if \code{diag = TRUE} or \code{union = TRUE}), the area of each rangemap; and in the upper triangle (if \code{union = TRUE}), the area of the pair-wise unions among the rangemaps.
}
\references{
Barbosa A.M. & Estrada A. (2016) Calcular corotipos sin dividir el territorio en OGUs: una adaptacion de los indices de similitud para su utilizacion directa sobre areas de distribucion. In: Gomez Zotano J., Arias Garcia J., Olmedo Cobo J.A. & Serrano Montes J.L. (eds.), Avances en Biogeografia. Areas de Distribucion: Entre Puentes y Barreras, pp. 157-163. Editorial Universidad de Granada & Tundra Ediciones, Granada (Spain)
}
\author{
A. Marcia Barbosa
}
\note{
This function uses the \pkg{PBSmapping} package to import and intersect the polygon rangemaps and to calculate areas. Remember to use projected rangemaps, preferably with an equal-area reference system, so that calculations are correct.
}
\seealso{
\code{\link{rangemapSim}}
}
