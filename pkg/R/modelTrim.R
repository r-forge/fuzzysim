modelTrim <-
  function(model, method = "summary", alpha = 0.05) {
    # version 1.9 (16 Nov 2018)

    stopifnot(
      class(model) %in% c("glm", "lm"),
      alpha > 0,
      alpha < 1
    )

    dat <- as.data.frame(model$model)  # new
    n.vars.start <- length(model$coefficients) - 1
    names.vars.start <- names(model$coefficients)[-1]

    if (method == "summary") {
      p.values <- expression(summary(model)$coefficients[ , 4])
    } else if (method == "anova") {
      p.values <- expression(as.matrix(anova(model, test = "Chi"))[ , 5])
    } else stop ("'method' must be either 'summary' or 'anova'")

    while (max(eval(p.values)[-1]) > alpha) {  # excludes p-value of intercept
      exclude <- names(which.max(eval(p.values)[-1]))
      model <- update(model, as.formula(paste("~.-", exclude)), data = dat)
      if (length(model$coefficients) == 1) {  # only intercept remains
        message("No significant variables left in the model.")
        break
      }  # end if length
    } # end while

    n.vars.trim <- length(model$coefficients) - 1
    excluded.vars <- setdiff(names.vars.start, names(model$coefficients)[-1])
    message(n.vars.start - n.vars.trim, " variable(s) excluded by 'modelTrim' function\n ", paste(excluded.vars, collapse = ", "), "\n\n")

    return(model)
  }
