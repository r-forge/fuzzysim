getPreds <-
function(data, models, id.col = NULL, Y = FALSE, P = TRUE, Favourability = TRUE, incl.input = TRUE) {
  # version 1.5 (18 Set 2014)

  start.time <- Sys.time()

  stopifnot(
    is.data.frame(data),
    is.list(models),
    is.null(id.col) | id.col %in% (1 : ncol(data)),
    is.logical(Y),
    is.logical(P),
    is.logical(Favourability),
    is.logical(incl.input)
  )

  if (!Y & !P & !Favourability) stop("There are no predictions to get
if all Y, P and Favourability are set to FALSE.")

  input.data <- data

  keeP <- P  # keep P only if the user wants it
  if (Favourability)  P <- TRUE  # P is necessary to calculate Fav

  n.nulls <- length(models[sapply(models, is.null)])
  if (n.nulls > 0)  warning (n.nulls, " model(s) were NULL and therefore
          did not generate predictions")
  models <- models[!sapply(models, is.null)]
  n.models <- length(models)
  mod.count <- 0

  for (m in 1:n.models) {
    mod.count <- mod.count + 1
    mod.name <- names(models)[m]
    message("Predicting with model ", mod.count, " of " , n.models,
            " (", mod.name, ")...")
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
    data <- data[ , -(1:ncol(input.data))]
    if (!is.null(id.col)) {
      data <- data.frame(input.data[ , id.col], data)
      names(data)[1] <- names(input.data)[id.col]
    }
  }

  duration <- difftime(start.time, Sys.time())
  units <- attr(duration, "units")
  duration <- round(abs(as.numeric(duration)), 1)
  message("Finished in ", duration, " ", units)
  return(data)
}
