Fav <-
function(obs = NULL, pred = NULL, n1n0 = NULL, model = NULL, 
                sample.preval = NULL, method = "RBV", true.preval = NULL) {
  # version 1.2 (25 Mar 2014)
  # obs: a vector of 1-0 values of a modelled binary variable
  # pred: a vector of predicted probability values given e.g. by logistic regression (note: non-probabilistic predicted values are not suitable for the Fav function!)
  # n1n0: alternatively to obs, the total number of ones and zeros, in this order -- e.g. n1n0 = c(214, 317); ignored if the 'obs' vector is provided
  # sample.preval: alternatively to obs or n1n0, the prevalence (proportion of positive cases) of the modelled binary variable in the modelled data
  # model: alternatively to all of the above, a model object of class "glm" (if provided, will override any values provided in the arguments described above)
  # method: either "RBV" for the original Real, Barbosa & Vargas (2006) procedure, or AT for the modification proposed by Albert & Thuiller (2008) (but see Acevedo & Real 2012, Naturwissenschaften 99:515-522)
  # true.preval: the true prevalence (as opposed to sample prevalence); necessary if you want to use the AT method
  
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
