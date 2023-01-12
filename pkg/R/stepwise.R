stepwise <- function(data, sp.col, var.cols, id.col = NULL, family = binomial(link="logit"), direction = "both", test.in = "Rao", test.out = "LRT", p.in = 0.05, p.out = 0.1, trace = 1, simplif = TRUE, preds = FALSE, Favourability = FALSE, Wald = FALSE) {

  # version 1.1 (12 Jan 2023)

  # following lines taken from glm():
  if (is.character(family)) family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  tests <- c("none", "Rao", "LRT", "Chisq", "F")
  if (!(test.in %in% tests)) stop("Invalid 'test.in'.")
  if (!(test.out %in% tests)) stop("Invalid 'test.out'.")

  if (Wald == TRUE && trace > 1 && !requireNamespace("aod")) {
    Wald <- FALSE
    warning("'Wald=TRUE' requires having the 'aod' package installed; printing conventional summary instead.")
  }
  # if (Wald == TRUE && trace > 1 && !("aod" %in% .packages())) {
  #   Wald <- FALSE
  #   warning("'Wald=TRUE' requires having the 'aod' package loaded.\n  Printing conventional summary() instead.\n  Run 'library(aod)' first if you want summaryWald().")
  # }

  data <- as.data.frame(data)

  if (is.numeric(sp.col)) response <- names(data)[sp.col] else response <- sp.col
  if (is.numeric(var.cols)) vars <- names(data)[var.cols] else vars <- var.cols
  if (is.numeric(id.col)) id <- names(data)[id.col] else if (is.character(id.col)) id <- id.col

  n.init <- nrow(data)
  data <- na.omit(data[ , c(response, vars)])
  na.loss <- n.init - nrow(data)
  if (na.loss > 0) message(na.loss, " cases excluded due to missing values.")

  if (preds)  predictions <- data.frame(id = 1:nrow(data))

  if (direction %in% c("forward", "both")) {
    form_start <- as.formula(paste(response, "~1"))
    form_scope <- as.formula(paste("~", paste(vars, collapse = "+")))
  }
  else if (direction == "backward") form_start <- as.formula(paste(response, "~", paste(vars, collapse = "+")))
  else stop ("Invalid 'direction'.")

  mod <- glm(formula = form_start, family = family, data = data)

  if (!simplif)  steps <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))

  if (direction %in% c("forward", "both")) {
    addable <- add1(mod, scope = form_scope, test = test.in)

    step_n <- 0

    while (min(addable$Pr, na.rm = TRUE) < p.in)  {
      step_n <- step_n + 1
      addvar <- rownames(addable)[which.min(addable$Pr)]
      mod <- update(mod, as.formula(paste("~.+", addvar)), data = data)
      if (!simplif)  steps[step_n, ] <- c("in", addvar)
      if (trace > 1) cat("---------------------------------------------------\n")
      if (trace > 0) cat("Step", step_n, "\n+ adding  ", addvar, "\n")
      if (trace > 1) {
        if (Wald) print(summaryWald(mod))
        else print(summary(mod))
      }

      if (preds)  predictions[ , paste0("step", step_n)] <- predict(mod, data, type = "response")
      #scope_updated <- as.formula(paste("~", paste(rownames(addable)[-c(1, grep(addvar, rownames(addable)))], collapse = "+")))

      droppable <- drop1(mod, test = test.out)
      if (direction == "both") {
        while (max(droppable$Pr, na.rm = TRUE) > p.out) {
          dropvar <- rownames(droppable)[which.max(droppable$Pr)]
          if (dropvar != addvar) {  # if it's not the last added variable (else it will be added and dropped again and again)
            step_n <- step_n + 1
            mod <- update(mod, as.formula(paste("~.-", dropvar)), data = data)
            if (!simplif)  steps[step_n, ] <- c("out", dropvar)
            if (trace > 1) cat("---------------------------------------------------\n")
            if (trace > 0) cat("Step", step_n, "\n- dropping", dropvar, "\n")
            if (trace > 1) {
              if (Wald) print(summaryWald(mod))
              else print(summary(mod))
            }
            if (preds)  predictions[ , paste0("step", step_n)] <- predict(mod, data, type = "response")
            droppable <- drop1(mod, test = test.out)
          } else {  # end if dropvar != addvar
            break
            }
        }  # end while droppable
      }  # end if !forward
      if (nrow(addable) <= 2) break  # if the previous 'addable' had just the intercept and this last added variable (loop runs endlessly otherwise)
      addable <- add1(mod, scope = form_scope, test = test.in)
    }  # end while addable
  }  # end if !backward

  if (direction == "backward") {
    droppable <- drop1(mod, test = test.out)
    step_n <- 0
    while (max(droppable$Pr, na.rm = TRUE) > p.out) {
      step_n <- step_n + 1
      dropvar <- rownames(droppable)[which.max(droppable$Pr)]
      mod <- update(mod, as.formula(paste("~.-", dropvar)), data = data)
      if (!simplif)  steps[step_n, ] <- c("out", dropvar)
      if (trace > 1) cat("---------------------------------------------------\n")
      if (trace > 0) cat("Step", step_n, "\n- dropping", dropvar, "\n")
      if (trace > 1) {
        if (Wald) print(summaryWald(mod))
        else print(summary(mod))
      }
      if (preds)  predictions[ , paste0("step", step_n)] <- predict(mod, data, type = "response")
      droppable <- drop1(mod, test = test.out)
    }  # end while droppable
    #if (nrow(droppable) <= 0) break  # apparently not needed
  }  # end if !backward

  if (simplif)  return(mod)

  steps <- data.frame(1:nrow(steps), steps)
  names(steps) <- c("step", "direction", "variable")

  if (!preds) return(list(model = mod, steps = steps))

  predictions <- predictions[ , -1]  # was there just to initialize the data.frame
  if (!is.null(id.col))  predictions <- data.frame(data[ , id.col, drop = FALSE], predictions)
  if (Favourability) predictions[ , -1] <- data.frame(lapply(predictions[ , -1], function(x) x <- Fav(pred = x, sample.preval = prevalence(model = mod))))
  return(list(model = mod, steps = steps, predictions = predictions))
}
