\name{fuzzySim-package}
\alias{fuzzySim-package}
\alias{fuzzySim}
\docType{package}
\title{
Fuzzy Similarity in Species Distributions
}
\description{
Functions to compute fuzzy versions of species occurrence patterns based on presence-absence data (including inverse distance interpolation, trend surface analysis, and prevalence-independent favourability obtained from probability of presence), as well as pair-wise fuzzy similarity (based on fuzzy logic versions of commonly used similarity indices) among those occurrence patterns. Includes also functions for model consensus and comparison (fuzzy overlap and fuzzy similarity, loss or gain), and for data preparation such as obtaining unique abbreviations of species names, cleaning species occurrence records, gridding (thinning) point occurrence data onto raster maps, converting species lists (long format) to presence-absence tables (wide format), transposing part of a data frame, selecting relevant variables for models, assessing the false discovery rate, or analysing and dealing with multicollinearity. Includes also sample datasets for providing practical examples. A step-by-step illustrated tutorial is available from the package homepage (http://fuzzysim.r-forge.r-project.org).
}
\details{
\tabular{ll}{
Package: \tab fuzzySim\cr
Type: \tab Package\cr
Version: \tab 4.33\cr
Date: \tab 2025-03-21\cr
License: \tab GPL-3\cr
}
}
\author{
A. Marcia Barbosa

Maintainer: A. Marcia Barbosa <ana.marcia.barbosa@gmail.com>
}
\references{
Barbosa A.M. (2015) fuzzySim: applying fuzzy logic to binary similarity indices in ecology. Methods in Ecology and Evolution, 6: 853-858.
}
\keyword{ package }
\examples{
data(rotifers)

head(rotifers)


# add column with species name abbreviations:

rotifers$spcode <- spCodes(rotifers$species, sep.species = "_",
nchar.gen = 1, nchar.sp = 5, nchar.ssp = 0)

head(rotifers)


# convert species list (long format) to presence-absence table
# (wide format):

rotifers.presabs <- splist2presabs(rotifers, sites.col = "TDWG4",
sp.col = "spcode", keep.n = FALSE)

head(rotifers.presabs)


# get 3rd-degree spatial trend surface for some species distributions:

data(rotif.env)

names(rotif.env)

rotifers.tsa <- multTSA(rotif.env, sp.cols = 18:20,
coord.cols = c("Longitude", "Latitude"), id.col = 1)

head(rotifers.tsa)


# get inverse squared distance to presence for each species:

rotifers.isqd <- distPres(rotif.env, sp.cols = 18:20,
coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 2, inv = TRUE)

head(rotifers.isqd)


# get prevalence-independent environmental favourability models
# for each species:

data(rotif.env)

names(rotif.env)

rotifers.fav <- multGLM(data = rotif.env, sp.cols = 18:20,
var.cols = 5:17, id.col = 1, step = FALSE, trim = TRUE,
Favourability = TRUE)


# get matrix of fuzzy similarity between species distributions:

# either based on inverse squared distance to presence:
rot.fuz.sim.mat <- simMat(rotifers.isqd[ , -1], method = "Jaccard")

# or on environmental favourability for presence:
rot.fuz.sim.mat <- simMat(rotifers.fav$predictions[ , 5:7],
method = "Jaccard")

head(rot.fuz.sim.mat)


# transpose fuzzy rotifer distribution data to compare
# regional species composition rather than species' distributions:

names(rotifers.isqd)

rot.fuz.reg <- transpose(rotifers.fav$predictions, sp.cols = 5:7,
reg.names = 1)

head(rot.fuz.reg)


# get matrix of fuzzy similarity between (some) regions'
# species compositions:

reg.fuz.sim.mat <- simMat(rot.fuz.reg[ , 1:10], method = "Jaccard")

head(reg.fuz.sim.mat)
}
