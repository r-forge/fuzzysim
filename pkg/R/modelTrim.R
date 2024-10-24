modelTrim <- function(model, method = "summary", alpha = 0.05, verbosity = 2, phy = NULL) {
  # version 3.1 (24 Oct 2024)

  stopifnot(
    all(class(model) %in% c("glm", "lm", "phylolm")),
    method %in% c("summary", "anova"),
    alpha > 0,
    alpha < 1
  )

  #dat <- as.data.frame(model$model) # new
  n.vars.start <- length(model$coefficients) - 1
  names.vars.start <- names(model$coefficients)[-1]

  if (method != "summary" && methods::is(model, "phylolm")) {
    if (verbosity > 0) message("'method' not implemented for 'phylolm' models;\nusing method = 'summary' instead\n")
    method <- "summary"
  }

  if (method == "summary") {
    if (all(class(model) %in% c("glm", "lm")))
      family <- family(model) # new after J.C.Guerrero email 30/12/2022
    p.values <- expression(summary(model)$coefficients[ , 4]) # column 4 named 'Pr(>|z|)' in glm, 'p.value' in phylolm
  } else if (method == "anova") {
    p.values <- expression(as.matrix(anova(model, test = "Chi"))[ , 5])
  }

  while (max(eval(p.values)[-1]) > alpha) { # excludes p-value of intercept
    exclude <- names(which.max(eval(p.values)[-1]))

    if (all(class(model) %in% c("glm", "lm")))
      model <- update(model, as.formula(paste("~.-", exclude)), data = model$model)

    else if (methods::is(model, "phylolm")) {
      d <- data.frame(model$y, model$X[ , -1, drop = FALSE]) # excludes intercept
      # names(d)[1] <- trimws(strsplit(model$formula, " ~ ")[[1]][1])
      # names(d)[1] <- as.character(as.formula(model$formula))[2]
      names(d)[1] <- all.vars(as.formula(model$formula))[1]
      terms_remaining <- names(d)[-grep(exclude, names(d))][-1]
      if (length(terms_remaining) == 0) terms_remaining <- "1"
      form <- reformulate(termlabels = terms_remaining, response = names(d)[1])
      model <- phylolm::phylolm(form, data = d, phy = phy)
    }

    if (length(model$coefficients) == 1) { # only intercept remains
      if (verbosity > 0) message("No significant variables left in model.")
      break
    } # end if length
  } # end while

  n.vars.trim <- length(model$coefficients) - 1
  excluded.vars <- setdiff(names.vars.start, names(model$coefficients)[-1])
  if (verbosity > 0) message(n.vars.start - n.vars.trim, " variable(s) excluded by 'modelTrim' function\n ", paste(excluded.vars, collapse = ", "), "\n\n")

  return(model)
}
