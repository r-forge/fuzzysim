spCodes <-
function(species, nchar.gen = 3, nchar.sp = 3, nchar.ssp = 0, sep.species = " ", sep.spcode = "", verbosity = 2) {
  # version 1.1 (2 May 2022)
  
  species <- as.character(unlist(species))
  splits <- strsplit(species, split = sep.species)
  gen <- sp <- vector("character", length(splits))
  for (i in 1:length(splits)) {
    gen[i] <- splits[[i]][1]
    sp[i] <- splits[[i]][2]
    if (is.na(sp[i]))  sp[i] <- ""
  }
  
  abbrev <- function (string, nchar) {
    abv <- substr(string, start = 1, stop = nchar)
    return(abv)
  }
  gen.code <- abbrev(gen, nchar.gen)
  sp.code <- abbrev(sp, nchar.sp)
  spcode <- paste(gen.code, sp.code, sep = sep.spcode)
  
  if (nchar.ssp > 0) {
    ssp <- vector("character", length(splits))
    for (i in 1:length(splits)) {
      ssp[i] <- splits[[i]][3]
      if (is.na(ssp[i]))  ssp[i] <- ""
    }
    ssp.code <- abbrev(ssp, nchar.ssp)
    spcode <- paste(spcode, ssp.code, sep = sep.spcode)
  }
  
  if (length(unique(species)) != length(unique(spcode))) {
    data <- cbind(species, spcode)
    unique.data <- data[!duplicated(data[ , c("species")]), ]
    ordered.data <- unique.data[order(unique.data[, "spcode"]), ]
    duplicates <- duplicated(ordered.data[ , "spcode"])
    for (i in 1:length(duplicates)) {
      if (isTRUE(duplicates[i])) duplicates[i-1] <- TRUE
    }  # to include the original as well as the duplicate
    ordered.data.duplicates <- ordered.data[duplicates, ]
    print(ordered.data.duplicates)
    stop("Resulting species codes are not unique (see above); try a different combination of nchar.gen, nchar.sp (and nchar.ssp), or check that you have specified the correct sep.species.")
  }
  if (verbosity > 0) cat("OK - no duplicated spcodes found.")
  return(spcode)
}
