multicol <- function(vars) {
  result <- matrix(NA, nrow = ncol(vars), ncol = 3)
  rownames(result) <- colnames(vars)
  colnames(result) <- c("Rsquared", "Tolerance", "VIF")
  for (v in 1:ncol(vars)) {
    v.name <- colnames(vars)[v]
    other.v.names <- colnames(vars)[-v]
    mod.formula <- as.formula(paste(v.name, "~", paste(other.v.names, collapse = "+")))
    mod <- lm(mod.formula, data = vars)
    R2 <- summary(mod) $ r.squared
    result[v, "Rsquared"] <- R2
    result[v, "Tolerance"] <- 1 - R2
    result[v, "VIF"] <- 1 / (1 - R2)
  }
  return(result)
}
