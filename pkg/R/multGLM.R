multGLM <- function(data, sp.cols, var.cols, id.col = NULL, family = "binomial",
                    test.sample = 0, FDR = FALSE, corSelect = FALSE, 
                    cor.thresh = 0.8, step = TRUE,  trace = 0, start = "null.model",
                    direction = "both", Y.prediction = FALSE, P.prediction = TRUE,
                    Favourability = TRUE, group.preds = TRUE,
                    trim = TRUE, ...) {

  # version 3.6 (20 Apr 2015)

  start.time <- Sys.time()
  input.ncol <- ncol(data)

  stopifnot (
    as.vector(na.omit(as.matrix(data[ , sp.cols]))) %in% c(0,1),
    sp.cols %in% (1 : input.ncol),
    var.cols %in% (1 : input.ncol),
    is.null(id.col) | id.col %in% (1 : input.ncol),
    family == "binomial",
    test.sample >= 0 | test.sample == "Huberty",
    length(test.sample) == 1  | (is.integer(test.sample) & test.sample > 0),
    length(test.sample) < nrow(data),
    is.logical(FDR),
    is.logical(step),
    start %in% c("null.model", "full.model"),
    direction %in% c("both", "backward", "forward"),
    is.logical(Y.prediction),
    is.logical(P.prediction),
    is.logical(Favourability),
    is.logical(group.preds),
    is.logical(trim),
    !Favourability | exists("Fav"),
    !trim | exists("modelTrim")
  )

  data$sample <- "train"
  n <- nrow(data)  # [is.finite(data[ , sp.cols]), ]  # but this can differ among spp
  data.row <- 1:n

  test.sample.input <- test.sample

  if (length(test.sample) == 1) {
    if (test.sample == "Huberty") {
      if (!FDR & !step & !trim) {
        test.sample <- percentTestData(length(var.cols)) / 100
        n.test <- round(n * test.sample)
        message(
          "Following Huberty's rule, ", test.sample * 100, "% of observations 
          (", n.test, " out of ", n, ") set aside for model testing; ", 
          n - n.test, " observations used for model training.")
      } else stop ("Sorry, Huberty's rule cannot be used with 'FDR', 'step' or 'trim', 
                   as these make the number of variables vary. 
                   Set these 3 parameters to FALSE, 
                   or use a different 'test.sample' option.")
    }  # end if Huberty
    else if (test.sample == 0) {
      message("All ", n, " observations used for model training; 
              none reserved for model testing.")
      n.test <- 0
    } else if (test.sample < 1) {
      n.test <- round(n * test.sample)
      message(
        test.sample * 100, "% of observations (", n.test, " out of ", n, ") set aside for model testing; ",
        n - n.test, " observations used for model training.")
    } else if (test.sample >= 1) {
      n.test <- test.sample
      message(
        n.test, " (out of ", n, ") observations set aside for model testing; ",
        n - n.test, " observations used for model training.")
    }
    test.sample <- sample(data.row, size = n.test, replace = FALSE)
      } else if (length(test.sample) > 1) {
        n.test <- length(test.sample)
        message(
          n.test, " (out of ", n, ") observations set aside for model testing; ",
          n - n.test, " observations used for model training.")
      }

  data$sample[data.row %in% test.sample] <- "test"
  train.data <- data[data$sample == "train", ]

  if (Favourability) {
    if (family != "binomial") {
      Favourability <- FALSE
      warning ("Favourability is only applicable to binomial responses,
              so it could not be calculated")
    }  # end if family != binomial (for when other families are implemented)
    }  # end if Fav

  keeP <- P.prediction  # keep P only if the user wants it
  if (Favourability)  P.prediction <- TRUE  # P is necessary to calculate Fav
  n.models <- length(sp.cols)
  n.preds <- n.models * (Y.prediction + P.prediction + Favourability)  # sums logical values of function arguments
  models <- vector("list", n.models)
  predictions <- matrix(NA, nrow = nrow(data), ncol = n.preds)
  colnames(predictions) <- rep("", n.preds)
  model.count <- 0
  pred.count <- 0
  attach(train.data, warn.conflicts = FALSE)  # won't work without attach

  for (s in sp.cols) {
    model.count <- model.count + 1
    response <- colnames(train.data)[s]
    message("\nBuilding model ", model.count, " of ", n.models,
            " (", response, ")...")
    cat(length(var.cols), "input predictor variable(s)\n\n")

    if (FDR) {
      fdr <- FDR(data = train.data, sp.cols = s, var.cols = var.cols, verbose = FALSE)
      if (nrow(fdr$select) == 0) {
        warning(paste0(
          "No variables passed the FDR test (so no variables included in the model)\n for '", response, "'. Consider using FDR = FALSE?"))
        #next
      } #else {
        cat(length(var.cols) - nrow(fdr$select), "variable(s) excluded by 'FDR' function\n", paste(row.names(fdr$exclude), collapse = ", "), "\n\n")
      #}
      sel.var.cols <- which(colnames(train.data) %in% rownames(fdr$select))
    }  # end if FDR
    else sel.var.cols <- var.cols  
    
    if (length(sel.var.cols) > 0 && corSelect == TRUE) {
      corselect <- corSelect(data = train.data, sp.cols = s, var.cols = sel.var.cols, cor.thresh = cor.thresh)
      corsel.var.cols <- corselect$selected.var.cols
      cat(length(sel.var.cols) - length(corsel.var.cols), "variable(s) excluded by 'corSelect' function\n", corselect$excluded.vars, "\n\n")
    }  # end if corSelect

    if (length(sel.var.cols) == 0)  model.vars <- 1
    else  model.vars <- colnames(train.data)[sel.var.cols]
    model.formula <- as.formula(paste(response, "~", paste(model.vars,
                                                           collapse = "+")))
    model.expr <- expression(glm(model.formula, family = binomial))

    if (step & length(sel.var.cols) > 0) {
      n.vars.start <- length(sel.var.cols)
      if (start == "full.model") {
        model <- step(eval(model.expr), direction = direction, trace = trace)
      } else if (start == "null.model") {
        model.scope <- model.formula[-2]  # removes response from formula
        null.formula <- as.formula(paste(response, "~", 1))
        model <- step(glm(null.formula, family = binomial),
                      direction = direction, scope = model.scope, trace = trace)
      } else stop ("'start' must be either 'full.model' or 'null.model'")
      n.vars.step <- length(model$coefficients) - 1
      excluded.vars <- setdiff(colnames(data[ , sel.var.cols]), names(model$coefficients)[-1])
      cat(n.vars.start - n.vars.step, "variable(s) excluded by 'step' function\n", paste(excluded.vars, collapse = ", "), "\n\n")
    } else model <- eval(model.expr)

    if (trim & length(sel.var.cols) > 0) {
      n.vars.start <- length(model$coefficients) - 1
      names.vars.start <- names(model$coefficients)[-1]
      model <- suppressMessages(modelTrim(model, ...))
      n.vars.trim <- length(model$coefficients) - 1
      excluded.vars <- setdiff(names.vars.start, names(model$coefficients)[-1])
      cat(n.vars.start - n.vars.trim, "variable(s) excluded by 'modelTrim' function\n", paste(excluded.vars, collapse = ", "), "\n\n")
    }

    if (step | trim) {
      sel.var.names <- names(model$coefficients)[-1]
      cat(length(sel.var.names), "variable(s) INCLUDED IN THE FINAL MODEL\n",
          paste(sel.var.names, collapse = ", "))
    }

    models[[model.count]] <- model
    names(models)[[model.count]] <- response

    if (Y.prediction) {
      pred.count <- pred.count + 1
      colnames(predictions)[pred.count] <- paste(response, "Y", sep = "_")
      predictions[ , pred.count] <- predict(model, data)
    }
    if (P.prediction) {
      pred.count <- pred.count + 1
      colnames(predictions)[pred.count] <- paste(response, "P", sep = "_")
      predictions[ , pred.count] <- predict(model, data, type = "response")
    }
    if (Favourability) {
      n1 <- sum(train.data[ , s] == 1, na.rm = TRUE)
      n0 <- sum(train.data[ , s] == 0, na.rm = TRUE)
      pred.count <- pred.count + 1
      predictions[ , pred.count] <- Fav(n1n0 = c(n1, n0), pred = predictions[ , pred.count - 1])
      colnames(predictions)[pred.count] <- paste(response, "F", sep = "_")
    } # end if Fav
  }  # end for s

  detach(train.data)
  #if (rm.null.models) models <- models[!sapply(models, is.null)]

  if (P.prediction & !keeP) {
    n.char <- nchar(colnames(predictions))
    pred.suffix <- substr(colnames(predictions), n.char - 1, n.char)
    P.cols <- grep("_P", pred.suffix)
    predictions <- predictions[ , - P.cols]
  }

  n.pred.types <- sum(Y.prediction, keeP, Favourability)
  if (n.pred.types == 0) {
    predictions <- data.frame()
  } else {
    predictions <- data.frame(data[ , id.col], sample = data[ , "sample"], predictions)

    if (n.pred.types == 1 | length(sp.cols) == 1)  group.preds <- FALSE

    if (group.preds) {
      first.pred.col <- ifelse(is.null(id.col), 2, 3) # 1st new col is 'sample'
      pred1.cols <- seq(first.pred.col, ncol(predictions), by = n.pred.types)
      pred2.cols <- seq(first.pred.col + 1, ncol(predictions), by = n.pred.types)
      pred3.cols <- NULL
      if (n.pred.types == 3) {
        pred3.cols <- seq(first.pred.col + 2, ncol(predictions),
                          by = n.pred.types)
      }  # end if pred.types > 2
      predictions <- data.frame(data[ , id.col],
                                sample = data$sample,
                                predictions[ , pred1.cols],
                                predictions[ , pred2.cols],
                                predictions[ , pred3.cols])
    }  # end if groups.preds

    if (!is.null(id.col)) {
      colnames(predictions)[1] <- colnames(data)[id.col]
    }

  }  # end if pred.types 0 else

  if (test.sample.input == 0)
    predictions <- predictions[ , - match("sample", colnames(predictions))]
  message("Finished!")
  timer(start.time)
  return(list(predictions = predictions, models = models))

  }  # end multGLM function
