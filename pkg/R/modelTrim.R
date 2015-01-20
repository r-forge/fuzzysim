modelTrim <-
function(model, method = "summary", alpha = 0.05) {
  # version 1.8 (16 Apr 2013)
  # model: a model object of class lm or glm
  # method: whether to use the significance of the coefficients (if method = 'summary', the default) or the significance of the variables themselves (if method = 'anova', better when there are categorical variables in the model).
  
  stopifnot(
    class(model) %in% c("glm", "lm"),
    alpha > 0,
    alpha < 1
  )
  
  if (method == "summary") {
    p.values <- expression(summary(model)$coefficients[ , 4])
  } else if (method == "anova") {
    p.values <- expression(as.matrix(anova(model, test = "Chi"))[ , 5])
  } else stop ("'method' must be either 'summary' or 'anova'")
  
  while (max(eval(p.values)[-1]) > alpha) {  # excludes p-value of intercept
    model <- update(model, as.formula(paste("~.-", names(which.max(eval(p.values)[-1])))))
    if (length(model$coefficients) == 1) {  # only intercept remains
      message("No significant variables left in the model.")
      break
    }  # end if length
  } # end while
  
  return(model)
}
