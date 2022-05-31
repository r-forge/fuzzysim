Fav <- function(model = NULL, obs = NULL, pred = NULL, n1n0 = NULL, sample.preval = NULL, method = "RBV", true.preval = NULL, verbosity = 2) {
  # version 1.8 (31 May 2022)
  
  if (!is.null(model)) {
    if (verbosity > 0) {
      if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
      if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
      if (!is.null(n1n0)) message("Argument 'n1n0' ignored in favour of 'model'.")
      if (!is.null(sample.preval)) message("Argument 'sample.preval' ignored in favour of 'model'.")
    }  # end if verbosity
    
    #   if (is(model, "glm") || is(model, "gam")) {
    #     obs <- model$y
    #     pred <- model$fitted.values
    #   } else if (is(model, "gbm")) {
    #     obs <- model$data$y
    #     exp_y <- exp(model$fit)  # gbm provides 'fit' on the predictors' scale
    #     pred <- exp_y / (1 + exp_y)  # logit to probability
    #   } else if (is(model, "randomForest")) {
    #     obs <- as.integer(as.character(model$y))
    #     pred <- predict(model, type = "prob")[ , "1"]
    #   } else if (is(model, "bart")) {
    #     if (is.null(model$fit$data)) stop("'$fit$data' section not available in 'model'. Try computing the 'bart' model with 'keeptrees=TRUE', or providing other arguments here instead of 'model', i.e. 'pred' plus one of 'obs', 'n1n0' or 'sample.preval'.")
    #     obs <- model$fit$data@y  # requires model ran with keeptrees=TRUE
    #     pred <- stats::fitted(model, type = "response")
    #   } else stop("'model' is of a non-implemented class.")
    
    obspred <- modEvA::mod2obspred(model)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]
  }  # end if model
  
  # if(!is.null(obs) & !is.null(pred) & !class(pred) == "RasterLayer" & length(obs) != length(pred)) {
  #   stop("'obs' and 'pred' must have the same length (and be in the same order).")
  # }
  
  #if (!is.vector(obs)) stop("'obs' must be of class 'vector'.")
  #if (!(is.vector(pred) || inherits(pred, "SpatRaster") || inherits(rast(pred), "SpatRaster"))) stop("'pred' must be of class 'vector', 'Raster' or 'SpatRaster'.")
  obs <- unlist(obs)  # in case input was tibble
  pred <- unlist(pred)  # in case input was tibble
  
  if (any(na.omit(pred[]) < 0) || any(na.omit(pred[]) > 1)) stop ("'pred' contains values that are not between 0 and 1, as probability must be.")  # '[]' in case 'pred' is raster
  
  vals <- na.omit(obs)
  if (is(model, "randomForest"))  vals <- as.integer(as.character(vals))
  if (!all(vals %in% c(0, 1))) stop("Favourability is only applicable when the response variable is binary, taking only values of 0 or 1.")
  
  if (!is.null(obs)) {
    n1 <- sum(obs == 1, na.rm = TRUE)
    n0 <- sum(obs == 0, na.rm = TRUE)
  }  else if (!is.null(n1n0)) {
    if (verbosity > 0) {
      if (!is.null(obs)) message("Argument 'n1n0' ignored in favour of 'obs'")
    }
    n1 <- n1n0[1]
    n0 <- n1n0[2]
    #if(n1 + n0 != length(pred)) stop("n1+n0 must equal the length of 'pred'.")
  } else if (!is.null(sample.preval)) {
    if (verbosity > 0) {
      if (!is.null(obs)) message("Argument 'sample.preval' ignored in favour of 'obs'")
      else if (!is.null(n1n0)) message("Argument 'sample.preval' ignored in favour of 'n1n0'")
    }
    n1 <- sample.preval * 100
    n0 <- 100 - n1
  } else stop("You need to provide either 'model', or 'pred' plus either one of 'obs', 'n1n0' or 'sample.preval'.")
  
  # slightly reduce probabilities of exactly 1, which would cause division by zero (resulting favourability is still 1):
  pred[pred == 1] <- 1 - 2.2e-16
  
  if (method == "RBV") {  # Real, Barbosa & Vargas 2006
    fav <- (pred / (1 - pred)) / ((n1 / n0) + (pred / (1 - pred)))
  } else if (method == "AT") {  # Albert & Thuiller (2008); but see Acevedo & Real (2012)!
    sample.preval <- n1 / (n1 + n0)
    fav <- (pred / (1 - pred)) /
      ((sample.preval / true.preval) + (pred / (1 - pred)))
  } else stop("method must be either 'RBV' or 'AT'")
  
  if (is.vector(fav) && !is.null(names(fav)))  names(fav) <- NULL  # in case input was tibble
  return(fav)
}
