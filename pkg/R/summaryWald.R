summaryWald <- function(model, interceptLast = TRUE) {
  # version 1.1 (12 Jan 2023)

  if (!requireNamespace("aod")) {
    stop("This function requires installing the 'aod' package.")
  }

  if (!inherits(model, "glm")) stop("'model' must be of class 'glm'.")

  coefs <- as.data.frame(summary(model)$coefficients)

  # coefs[ , "z value"] <- coefs[ , "z value"] ^ 2  # Wald = z^2: https://www.listendata.com/2015/10/calculate-wald-chi-square-mathematically.html (tip by Alba Estrada)
  # colnames(coefs) <- sub("z value", "Wald", colnames(coefs))
  # colnames(coefs) <- sub("Pr\\(>\\|z\\|\\)", "Sig.", colnames(coefs))
  # but the Wald significance is only the same as Pr(>|z|) for binary GLMs; Poisson gave different p-values

  coefs <- coefs[ , -grep("z", names(coefs))]  # remove z test (to which remaining statistics won't correspond)
  coefs$Wald <- NA
  coefs$Sig. <- NA
  coefs$`Exp(B)` <- exp(coefs$Estimate)
  for (v in 1:nrow(coefs)) {
    w <- aod::wald.test(Sigma = vcov(model), b = coef(model), Terms = v)
    coefs[v, "Wald"] <- w$result$chi2["chi2"]
    coefs[v, "Sig."] <- w$result$chi2["P"]
  }

  if (interceptLast && nrow(coefs) > 1) coefs <- coefs[c(2:nrow(coefs), 1), ]  # intercept in last row

  return(coefs)
}
