getPreds <-
function(data, models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE, incl.input = FALSE) {
  # version 1.9 (30 Jan 2020)

  if (!Y & !P & !Favourability) stop("There are no predictions to get if all Y, P and Favourability are set to FALSE.")
  
  start.time <- Sys.time()
  
  if (is(data, "RasterStack")) {  # previously 'if("raster" %in% class(data))'
    if (!requireNamespace("raster")) stop("Input 'data' is in raster format, so you need to install the 'raster' package first.")
    preds <- raster::stack()
    mod.count <- 0
      
    for (m in 1:length(models)) {
      mod.count <- mod.count + 1
      mod <- models[[m]]
      mod.name <- names(models)[m]
      message("Predicting with model ", mod.count, " of " , length(models), " (", mod.name, ")...")

      if (Y == TRUE) {
        preds <- raster::addLayer(preds, raster::predict(data, mod))
        names(preds)[raster::nlayers(preds)] <- paste(mod.name, "Y", sep = "_")
      }  # end if Y
      
      if (P == TRUE | Favourability == TRUE) {
        p <- raster::predict(data, mod, type = "response")
        
        if (P == TRUE) {
          preds <- raster::addLayer(preds, p)
          names(preds)[raster::nlayers(preds)] <- paste(mod.name, "P", sep = "_")
        }  # end if P
      }  # end if P or F
      
      if (Favourability == TRUE) {
        n1 <- sum(mod$y == 1)
        n0 <- sum(mod$y == 0)
        preds <- raster::addLayer(preds, (p/(1 - p))/((n1/n0) + (p/(1 - p))))
        names(preds)[raster::nlayers(preds)] <- paste(mod.name, "F", sep = "_")
      }  # end if Fav
    }  # end for m
    
    return(preds)  
  }  # end if RasterStack
  
  stopifnot(
    is.data.frame(data) || is(data, "RasterStack"),
    is.list(models),
    is.null(id.col) | id.col %in% (1 : ncol(data)),
    is.logical(Y),
    is.logical(P),
    is.logical(Favourability),
    is.logical(incl.input)
  )

  input.data <- data

  keeP <- P  # keep P only if the user wants it
  if (Favourability)  P <- TRUE  # P is necessary to calculate Fav

  n.nulls <- length(models[sapply(models, is.null)])
  if (n.nulls > 0)  warning (n.nulls, " model(s) were NULL and therefore did not generate predictions")
  models <- models[!sapply(models, is.null)]
  n.models <- length(models)
  mod.count <- 0

  for (m in 1:n.models) {
    mod.count <- mod.count + 1
    mod.name <- names(models)[m]
    message("Predicting with model ", mod.count, " of " , n.models, " (", mod.name, ")...")
    if (Y) {
      data[ , ncol(data) + 1] <- predict(models[[mod.count]], data)
      names(data)[ncol(data)] <- paste(mod.name, "Y", sep = "_")
    }
    if (P) {
      data[ , ncol(data) + 1] <- predict(models[[mod.count]], data,
                                         type = "response")
      names(data)[ncol(data)] <- paste(mod.name, "P", sep = "_")
    }
    if (Favourability) {
      data[ , ncol(data) + 1] <- Fav(pred = data[ , ncol(data)],
                                   n1n0 = c(sum(models[[mod.count]]$y == 1),
                                            sum(models[[mod.count]]$y == 0)))
      names(data)[ncol(data)] <- paste(mod.name, "F", sep = "_")
      if (!keeP) data <- data[ , -(ncol(data) - 1)]
    }  # end if Fav
  }  # end for m

  if (incl.input) {
    id.col <- NULL
  } else {
    data <- data[ , -(1:ncol(input.data)), drop = FALSE]
    if (!is.null(id.col)) {
      data <- data.frame(input.data[ , id.col, drop = FALSE], data)
      names(data)[1] <- names(input.data)[id.col]
    }
  }

  message("Finished!")
  timer(start.time)
  return(data)
}
