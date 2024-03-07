stepByStep <- function(data,
                       sp.col,
                       var.cols,
                       family = binomial(link = "logit"),
                       Favourability = FALSE,
                       trace = 0,
                       direction = "both",  # argument implemented by Alba Estrada
                       select = "AIC", k = 2,
                       test.in = "Rao", test.out = "LRT",
                       p.in = 0.05, p.out = 0.1,
                       cor.method = "pearson") {

  # version 2.0 (6 Mar 2023)

  if (length(sp.col) > 1)  stop("Sorry, this function is implemented for only one response variable at a time, so 'sp.col' must indicate only one column")

  if (direction == "backward")  stop ("Sorry, 'backward' direction is not implemented in this function. Use either 'forward' or 'both'.")

  stopifnot (
    direction %in% c("forward", "both"),
    select %in% c("AIC", "BIC", "p.value")
  )

  if (is(data, "glm"))  mod.in <- TRUE  else  mod.in <- FALSE

  if (mod.in) {  # new 5 Mar 2023

    mod <- data
    data <- mod$model
    response <- names(mod$model)[1]
    vars <- names(mod$model)[-1]

  } else {  # if !is(data, "glm")

    data <- as.data.frame(data)

    n.init <- nrow(data)
    data <- na.omit(data)
    na.loss <- n.init - nrow(data)
    if (na.loss > 0) message(na.loss, " cases excluded due to missing values.")

    if (is.numeric(sp.col)) response <- names(data)[sp.col] else response <- sp.col
    if (is.numeric(var.cols)) vars <- names(data)[var.cols] else vars <- var.cols

  }  # end if is(data, "glm") else

  if (select == "BIC") {
    n <- nrow(data)
    k <- log(n)
  }

  null.model.formula <- reformulate(termlabels = "1", response = response)
  scope.formula <- reformulate(termlabels = vars)

  if (!mod.in) {
    if (select %in% c("AIC", "BIC"))  mod <- step(glm(null.model.formula, family = family, data = data), scope = scope.formula, direction = direction, trace = trace, k = k)
    else if (select == "p.value") {
      mod <- stepwise(data = data, sp.col = response, var.cols = vars, family = family, direction = direction, trace = trace, test.in = test.in, test.out = test.out, p.in = p.in, p.out = p.out, simplif = FALSE, preds = FALSE, Favourability = FALSE, Wald = FALSE)
      steps <- mod$steps
      mod <- mod$model
    }
  }

  pred.final <- mod$fitted.values

  if (Favourability == TRUE && !all(c("binomial", "logit") %in% mod$family)) {
    Favourability <- FALSE
    warning("'Favourability' is only applicable when family=binomial(link='logit'), so it was automatically set to FALSE.")
  }
  if (Favourability)  fav.final <- Fav(model = mod)

  if (mod.in) {  # new 5-6 Mar 2023
    model.vars <- names(mod$coefficients)[-grep("(Intercept)", names(mod$coefficients))]
    model.vars <- paste("+", model.vars)
  } else {  # if !mod.in
    if (select == "p.value") {
      var.entry <- ifelse(steps[ , "direction"] == "in", "+", "-")
      model.vars <- paste(var.entry, steps$variable)
    } else {  # if select != "p.value"
      # model.vars.split <- sapply(mod$anova[ , 1], strsplit, split = " ")
      # model.vars <- lapply(model.vars.split, `[`, 2)
      # model.vars <- as.character(model.vars)[-1]
      model.vars <- mod$anova[ , 1][nchar(mod$anova[ , 1]) > 0]  # changed by Alba Estrada to accommodate direction="both"
    }
  }

  n.steps <- length(model.vars)

  preds <- favs <- as.data.frame(matrix(nrow = nrow(data), ncol = n.steps))
  cor.P <- cor.F <- vector("numeric", n.steps)
  names(model.vars) <- names(preds) <- names(favs) <- names(cor.P) <- names(cor.F) <- paste0("step", 1:n.steps)

  for (s in 1:n.steps) {
    step.vars <- model.vars[1:s]
    #mod.step <- glm(as.formula(paste(response, "~", paste(step.vars, collapse = "+"))), family = family, data = data)
    mod.step <- glm(as.formula(paste(response, "~", paste(step.vars, collapse = ""))), family = family, data = data)  # changed by Alba Estrada to accommodate direction="both"
    if (Favourability) {
      favs[ , s] <- Fav(model = mod.step)
      cor.F[s] <- cor(favs[ , s], fav.final)
    } else {
      preds[ , s] <- mod.step$fitted.values
      cor.P[s] <- cor(preds[ , s], pred.final, method = cor.method)
    }
  }; rm(s)

  if (Favourability)  result <- list(predictions = favs, correlations = cor.F, variables = model.vars, model = mod)
  else result <- list(predictions = preds, correlations = cor.P, variables = model.vars, model = mod)

  return(result)
}
