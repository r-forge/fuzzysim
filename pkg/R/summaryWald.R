summaryWald <- function(model, interceptLast = TRUE) {
  
  if (!requireNamespace("aod")) {
    stop("This function requires installing the 'aod' package.")
  }
  
  if (!inherits(model, "glm")) stop("'model' must be of class 'glm'.")
  
  coefs <- as.data.frame(summary(model)$coefficients)
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
