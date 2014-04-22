\name{fuzzySim-package}
\alias{fuzzySim-package}
\alias{fuzzySim}
\docType{package}
\title{
Fuzzy similarity in species distributions
}
\description{
Functions to calculate fuzzy versions of species' occurrence patterns based on 
presence-absence data, and pair-wise fuzzy similarity (based on fuzzy versions 
of commonly used similarity indices) among those occurrence patterns. Includes 
also functions for data preparation, such as obtaining unique abbreviations of 
species names, converting species lists (long format) to presence-absence 
tables (wide format), or transposing part of a data frame. Complete with a 
couple of sample data sets for providing practical examples.
}
\details{
\tabular{ll}{
Package: \tab fuzzySim\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-03-19\cr
License: \tab GPL-3\cr
}
}
\author{
A. Marcia Barbosa

Maintainer: A. Marcia Barbosa <barbosa@uevora.pt>
}
\references{
Barbosa A.M. (submitted) fuzzySim: A Fuzzy Implementation of Binary Similarity 
Indices for Spatially Aware Analysis of Species' Distributional Associations.
}
\keyword{ package }
\seealso{
\code{RMACOQUI} (Olivero, Real & Marquez 2011, Systematic Biology 60:645-60, 
doi 10.1093/sysbio/syr026)
}
\examples{
data(rotifers)

head(rotifers)


# add column with species name abbreviations:

rotifers$spcode <- spCodes(rotifers$species, sep.species = "_", nchar.gen = 1, 
nchar.sp = 5, nchar.ssp = 0)

head(rotifers)


# convert species list (long format) to presence-absence table (wide format):

rotifers.presabs <- splist2presabs(rotifers, sites.col = "LEVEL3_COD", 
sp.col = "spcode", keep.n = FALSE)

head(rotifers.presabs)


# get 3rd-degree spatial trend surface for each species' distribution:

data(rotifers01)

names(rotifers01)

rotifers.tsa <- multTSA(rotifers01, sp.cols = 6:40, 
coord.cols = c("Long", "Lati"), id.col = 1)

head(rotifers.tsa)


# get fuzzy version of presence-absence using inverse squared 
# distance to presence:

rotifers.dsq <- distPres(rotifers01, sp.cols = 6:ncol(rotifers01), 
coord.cols = c("Long", "Lati"), id.col = 1, p = 2, inv = TRUE)

head(rotifers.dsq)


# get matrix of fuzzy similarity between rotifer species distributions:

rot.fuz.sim.mat <- simMat(rotifers.dsq[ , -1], method = "Baroni")

head(rot.fuz.sim.mat)


# transpose fuzzy rotifer distribution data to compare 
# regional species composition rather than species' distributions:

names(rotifers.dsq)

rot.fuz.reg <- transpose(rotifers.dsq, sp.cols = 2:ncol(rotifers.dsq), 
reg.names = 1)

head(rot.fuz.reg)


# get matrix of fuzzy similarity between (some) regions' species compositions:

reg.fuz.sim.mat <- simMat(rot.fuz.reg[ , 1:100], method = "Baroni")

head(reg.fuz.sim.mat)
}
