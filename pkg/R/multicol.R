multicol <- function(vars = NULL, model = NULL, reorder = TRUE) {
  
  if (is.null(vars)) {
    if (is.null(model)) stop ("You must provide either 'vars' or 'model'.")
    if (!("glm" %in% class(model))) stop ("'model' must be an object of class 'glm'.")
    vars <- model$model[ , -1]
  }
  
  if (!(all(class(vars) %in% c("matrix", "data.frame")))) stop ("'vars' must be a matrix or data frame")
  vars <- as.data.frame(vars)
  if (ncol(vars) < 2)  return (message("Cannot calculate collinearity with less than two variables."))
  
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
  result <- data.frame(result)
  if (reorder)  result <- result[order(result$VIF, decreasing = TRUE), ]
  return(result)
}
