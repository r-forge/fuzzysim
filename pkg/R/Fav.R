Fav <-
function(model = NULL, obs = NULL, pred = NULL, n1n0 = NULL, 
         sample.preval = NULL, method = "RBV", true.preval = NULL) {
  # version 1.3 (18 Oct 2019)

  if (!is.null(model)) {
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(n1n0)) message("Argument 'n1n0' ignored in favour of 'model'.")
    if (!is.null(sample.preval)) message("Argument 'sample.preval' ignored
in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model

  if(!is.null(obs) & !is.null(pred) & length(obs) != length(pred)) {
    stop("'obs' and 'pred' must have the same length (and be in the same order).")
  }

  if (!is.null(obs)) {
    n1 <- sum(obs == 1, na.rm = TRUE)
    n0 <- sum(obs == 0, na.rm = TRUE)
  }  # end if obs

  else if (!is.null(n1n0)) {
    if(!is.null(model)) message("Argument 'n1n0' ignored in favour of 'model'")
    else if (!is.null(obs)) message("Argument 'n1n0' ignored in favour of 'obs'")
    n1 <- n1n0[1]
    n0 <- n1n0[2]
    #if(n1 + n0 != length(pred)) stop("n1+n0 must equal the length of 'pred'.")
  }  # end if n1n0

  else if (!is.null(sample.preval)) {
    if(!is.null(model)) message("Argument 'sample.preval' ignored
                                in favour of 'model'")
    else if (!is.null(obs)) message("Argument 'sample.preval' ignored
                                    in favour of 'obs'")
    else if (!is.null(n1n0)) message("Argument 'sample.preval' ignored
                                     in favour of 'n1n0'")
    n1 <- sample.preval * 100
    n0 <- 100 - n1
  }  # end if sample.preval

  else stop("You need to provide either 'model', or 'obs' plus either one of
            'pred', 'n1n0' or 'sample.preval'.")
  
  # slightly reduce probabilities of exactly 1, which would cause division by zero:
  # (resulting favourability is still 1)
  pred[pred == 1] <- 1 - 2.2e-16
  
  if(method == "RBV") {  # Real, Barbosa & Vargas 2006
    fav <- (pred / (1 - pred)) / ((n1 / n0) + (pred / (1 - pred)))
  }

  else if(method == "AT") {  # Albert & Thuiller 2008; cf. Acevedo & Real 2012
    sample.preval <- n1 / (n1 + n0)
    fav <- (pred / (1 - pred)) /
      ((sample.preval / true.preval) + (pred / (1 - pred)))
  }

  else stop("method must be either 'RBV' or 'AT'")

  return(fav)
}
