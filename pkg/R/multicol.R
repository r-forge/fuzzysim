multicol <- function(vars = NULL, model = NULL, reorder = TRUE, vif.thresh = Inf, max.nvars = Inf, verbosity = 2, simplif = FALSE, plot = FALSE, ...) {

  # version 1.5 (22 Apr 2026)

  if (is.null(vars)) {
    if (is.null(model)) stop ("You must provide either 'vars' or 'model'.")
    if (!("glm" %in% class(model))) stop ("'model' must be an object of class 'glm'.")
    vars <- model$model[ , -1]
  }

  #if (!(all(class(vars) %in% c("matrix", "data.frame")))) stop ("'vars' must be a matrix or data frame")
  vars <- as.data.frame(vars)
  if (ncol(vars) < 2)  return (message("Cannot compute collinearity with just one variable."))

  multic <- function(vars) {
    out <- matrix(NA, nrow = ncol(vars), ncol = 3)
    rownames(out) <- colnames(vars)
    colnames(out) <- c("Rsquared", "Tolerance", "VIF")
    for (v in 1:ncol(vars)) {
      v.name <- colnames(vars)[v]
      other.v.names <- colnames(vars)[-v]
      mod.formula <- as.formula(paste(v.name, "~", paste(other.v.names, collapse = "+")))
      mod <- lm(mod.formula, data = vars)
      R2 <- summary(mod) $ r.squared
      out[v, "Rsquared"] <- R2
      out[v, "Tolerance"] <- 1 - R2
      out[v, "VIF"] <- 1 / (1 - R2)
    }
    return(data.frame(out))
  }  # end multic function

  repeat {
    result <- multic(vars)
    if ((max(result$VIF, na.rm = TRUE) < vif.thresh) &&
        (ncol(vars) <= max.nvars)) break
    exclude <- rownames(result)[which.max(result$VIF)]
    if (verbosity > 0) {
      message(paste0("removing ", exclude,
                     " (VIF = ", round(result[exclude, "VIF"], 3), ")"))
    }
    vars[ , exclude] <- NULL
    if (ncol(vars) < 2) break
  }

  if (reorder)  result <- result[order(result$VIF, decreasing = TRUE), ]

  if (plot) {
    modEvA::lollipop(result$VIF, names = rownames(result), ...)
  }

  if (simplif) return(rownames(result))
  return(result)
}
