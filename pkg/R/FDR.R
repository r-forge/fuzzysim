FDR <-
function(data = NULL, sp.cols = NULL, var.cols = NULL, pvalues = NULL,
         model.type, family = "binomial", correction = "fdr", q = 0.05,
         verbose = TRUE) {

  # version 3.2 (26 May 2014)

  if (length(sp.cols) > 1) stop ("Sorry, FDR is currently implemented for only one response variable at a time, so 'sp.cols' must indicate only one column")
  if (missing(model.type)) stop ("'model.type' is missing; specify either 'LM' or 'GLM'")
  if (!(correction %in% p.adjust.methods)) stop("Invalid correction method.
Type 'p.adjust.methods' for available options.")

  response <- data[ , sp.cols]
  predictors <- data[ , var.cols]

  if (!is.null(pvalues)) {
    coeffs <- FALSE
    p.bivar <- pvalues[ , 2]
    names(p.bivar) <- pvalues[ , 1]
  }  # end if pvalues

  else {
    coeffs <- TRUE
    if (is.null(ncol(predictors)))
      stop("You need more than one predictor to calculate the FDR.")

    p.bivar <- coef.bivar <- vector("numeric", length = ncol(predictors))

    for (i in 1:length(p.bivar)) {
      if(model.type == "GLM") {
        model <- glm(response ~ predictors[ , i], family = family)
        p.bivar[i] <- anova(model, test = "Chi") [ , "Pr(>Chi)"] [2]
        coef.bivar[i] <- model[["coefficients"]][2]
      }  # end if GLM

      else if (model.type == "LM") {
        model <- lm(response ~ predictors[ , i])
        p.bivar[i] <- anova(model, test = "Chi") [ , "Pr(>F)"] [1]
        coef.bivar[i] <- model[["coefficients"]][2]
      }  # end if LM

      else stop("'model.type' must be either 'LM' or 'GLM'")
    } # end for i in p.bivar

    if (is.na(p.bivar[i])) message("A p-value could not be calculated for var.col number", i)

  }  # end else

  if (coeffs) {
    results <- data.frame(cbind(coef.bivar, p.bivar),
                          row.names = names(predictors))
    names(results) <- c("bivariate.coeff", "p.value")
    results <- results[order(results[ , "p.value"]), ]
    results[ , "p.adjusted"] <- p.adjust(results[ , "p.value"], method = correction)
  } else {
    results <- data.frame(p.value = p.bivar, row.names = pvalues[ , 1])
    results <- data.frame(p.value = results[order(results[ , "p.value"]), ])
    results[ , "p.adjusted"] <- p.adjust(results[ , "p.value"], method = correction)
  }  # end if coeffs else

  p.adjusted <- NULL  # avoids R CMD check warning about no visible binding for variable 'p.adjusted'
  exclude <- subset(results, p.adjusted > q)
  select <- subset(results, p.adjusted <= q)
  if (verbose) cat("\nBivariate p-values adjusted with '", correction,
      "' correction;\n", nrow(exclude), " variables excluded, ",
      nrow(select), " selected\n\n", sep = "")

  return(list(exclude = exclude, select = select))
}