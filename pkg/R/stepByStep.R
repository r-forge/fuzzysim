stepByStep <- function(data,
                       sp.col,
                       var.cols,
                       family = binomial(link = "logit"),
                       Favourability = FALSE,
                       trace = 0,
                       direction = "forward",  # argument implemented by Alba Estrada; "forward" default for back-compatibility
                       k = 2,
                       cor.method = "pearson") {

  # version 1.8 (5 Mar 2023)

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

  null.model.formula <- reformulate(termlabels = "1", response = response)
  scope.formula <- reformulate(termlabels = vars)

  if (!mod.in)  mod <- step(glm(null.model.formula, family = family, data = data), scope = scope.formula, direction = direction, trace = trace, k = k)

  pred.final <- mod$fitted.values

  if (Favourability == TRUE && !all(c("binomial", "logit") %in% mod$family)) {
    Favourability <- FALSE
    warning("'Favourability' is only applicable when family=binomial(link='logit'), so it was automatically set to FALSE.")
  }
  if (Favourability)  fav.final <- Fav(model = mod)

  if (mod.in) {  # new 5 Mar 2023
    model.vars <- names(mod$coefficients)[-grep("(Intercept)", names(mod$coefficients))]
    model.vars <- paste("+", model.vars)  # matching the step() output below
  } else {
  # model.vars.split <- sapply(mod$anova[ , 1], strsplit, split = " ")
  # model.vars <- lapply(model.vars.split, `[`, 2)
  # model.vars <- as.character(model.vars)[-1]
  model.vars <- mod$anova[ , 1][nchar(mod$anova[ , 1]) > 0]  # changed by Alba Estrada to accommodate direction="both"
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
