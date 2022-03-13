FDR <- function (data = NULL, sp.cols = NULL, var.cols = NULL, pvalues = NULL,
                 model.type = NULL, family = "auto", correction = "fdr", q = 0.05,
                 verbose = TRUE, simplif = FALSE)
  
  # version 3.8 (10 Mar 2022)
  
{
  if (length(sp.cols) > 1)
    stop("Sorry, FDR is currently implemented for only one response variable at a time, so 'sp.cols' must indicate only one column")
  
  if (!is.null(model.type))  warning ("Argument 'model.type' is deprecated and now ignored, as this info is included in 'family' (e.g. 'gaussian' for LM, 'binomial' or 'poisson' for GLM).")
  
  model.type <- "GLM"  # it's always GLM, even for LM; family is what may change
  
  # n.init <- nrow(data)
  # data <- data[is.finite(data[ , sp.cols]), ]
  # na.loss <- n.init - nrow(data)
  # if (na.loss > 0) message(na.loss, " cases excluded due to missing or non-finite values.")
  # -> MOVED FURTHER BELOW (if null pvalues)
  
  if (family == "auto" && is.null(pvalues)) {  # not all families are available in auto!
    vals <- which(is.finite(data[ , sp.cols]))
    if (all(data[vals, sp.cols] %in% c(0, 1)))  family <- "binomial"
    else if (all(data[vals, sp.cols] >= 0) && all(data[vals, sp.cols] %% 1 == 0))  family <- "poisson"
    else if (all(data[vals, sp.cols] >= 0))  family <- "Gamma"
    else family <- "gaussian"
    if (verbose) message("\nUsing generalized linear models of family '", family, "'.\n")
  }
  
  if (!(correction %in% p.adjust.methods))
    stop("Invalid correction method.\nType 'p.adjust.methods' for available options.")
  response <- data[, sp.cols]
  predictors <- data[, var.cols]
  
  if (!is.null(pvalues)) {
    if (!is.null(data) | !is.null(sp.cols) | !is.null(var.cols)) message("Argments 'data', 'sp.cols' and 'var.cols' are ignored when 'pvalues' is provided.")
    
    coeffs <- aic <- bic <- FALSE
    p.bivar <- pvalues[, 2]
    names(p.bivar) <- pvalues[, 1]
    
  } else {  # if null pvalues
    n.init <- nrow(data)
    data <- data[is.finite(data[ , sp.cols]), ]
    na.loss <- n.init - nrow(data)
    if (na.loss > 0) message(na.loss, " cases excluded due to missing or non-finite values.")
    
    coeffs <- aic <- bic <- TRUE
    if (is.null(ncol(predictors)))
      stop("You need more than one predictor to calculate the FDR.")
    p.bivar <- coef.bivar <- aic.bivar <- bic.bivar <- vector("numeric", length = ncol(predictors))
    for (i in 1:length(p.bivar)) {
      #if (model.type == "GLM") {
      model <- glm(response ~ predictors[, i], family = family)
      p.bivar[i] <- anova(model, test = "Chi")[, "Pr(>Chi)"][2]
      coef.bivar[i] <- model[["coefficients"]][2]
      #aic.bivar[i] <- model[["aic"]]
      aic.bivar[i] <- extractAIC(model, k = 2)[2]
      bic.bivar[i] <- extractAIC(model, k = log(nobs(model)))[2]
      #}
      #else if (model.type == "LM") {
      #  model <- lm(response ~ predictors[, i])
      #  p.bivar[i] <- anova(model, test = "Chi")[, "Pr(>F)"][1]
      #  coef.bivar[i] <- model[["coefficients"]][2]
      #  aic.bivar[i] <- extractAIC(model)[2]
      #}
      #else stop("'model.type' must be either 'LM' or 'GLM'")
      
      if (is.na(p.bivar[i]))
        message("A p-value could not be calculated for var.col number", i)
      if (is.na(aic.bivar[i]))
        message("AIC could not be calculated for var.col number", i)
      
      if (is.na(aic.bivar[i]))
        message("BIC could not be calculated for var.col number", i)
    }; rm(i)
  }  # end if null pvalues
  
  if (coeffs) {
    results <- data.frame(cbind(coef.bivar, aic.bivar, bic.bivar, p.bivar), row.names = names(predictors))
    names(results) <- c("bivariate.coeff", "AIC", "BIC", "p.value")
    results <- results[order(results[, "p.value"]), ]
    results[, "p.adjusted"] <- p.adjust(results[, "p.value"],
                                        method = correction)
    #results[, "symbol"] <- ""
    #results[, "symbol"] [results[, "p.adjusted"] < 0.1] <- "."
    #results[, "symbol"] [results[, "p.adjusted"] < 0.05] <- "*"
    #results[, "symbol"] [results[, "p.adjusted"] < 0.01] <- "**"
    #results[, "symbol"] [results[, "p.adjusted"] < 0.001] <- "***"
  } else {  # if !coeffs
    if (aic | bic)  results <- data.frame(AIC = aic.bivar, BIC = bic.bivar, p.value = p.bivar, row.names = pvalues[, 1])
    else results <- data.frame(p.value = p.bivar, row.names = pvalues[, 1])
    #results <- data.frame(p.value = results[order(results[, "p.value"]), ])
    results <- results[order(results[, "p.value"]), , drop = FALSE]
    results[, "p.adjusted"] <- p.adjust(results[, "p.value"],
                                        method = correction)
  }
  p.adjusted <- NULL
  
  if (simplif)  return (results)
  
  exclude <- subset(results, p.adjusted > q)
  select <- subset(results, p.adjusted <= q)
  
  if (verbose) {
    message("Bivariate p-values adjusted with '", correction,
            "' correction;\n", nrow(exclude), " variable(s) excluded, ",
            nrow(select), " selected (with q = ", q, ")\n")
  }
  list(exclude = exclude, select = select)
}
