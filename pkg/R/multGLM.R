multGLM <- function(data, sp.cols, var.cols, id.col = NULL, block.cols = NULL, family = "binomial", test.sample = 0, FDR = FALSE, test = "Chisq", correction = "fdr", FDR.first = TRUE, corSelect = FALSE, coeff = TRUE, cor.thresh = ifelse(isTRUE(coeff), 0.8, 0.05), cor.method = "pearson", step = TRUE, trace = 0, start = "null.model", direction = "both", select = "AIC", trim = TRUE, Y.prediction = FALSE, P.prediction = TRUE, Favourability = TRUE, group.preds = TRUE, TSA = FALSE, coord.cols = NULL, degree = 3, verbosity = 2, test.in = "Rao", test.out = "LRT", p.in = 0.05, p.out = 0.1, ...) {

  # version 5.8 (13 Mar 2024)

  if (!is.null(block.cols)) stop("Sorry, 'block.cols' is still under implementation and is not yet ready for use.")
  # block.cols may or may not be included in var.cols:
  # var.cols <- setdiff(var.cols, block.cols)

  start.time <- Sys.time()
  on.exit(timer(start.time))

  data <- as.data.frame(data)

  input.ncol <- ncol(data)
  input.names <- colnames(data)

  if (family != "binomial") stop ("Sorry, 'binomial' is the only family currently implemented.")

  stopifnot (
    as.vector(na.omit(as.matrix(data[ , sp.cols]))) %in% c(0, 1),
    all(sp.cols %in% (1 : input.ncol)) || all(sp.cols %in% input.names),
    all(var.cols %in% (1 : input.ncol)) || all(var.cols %in% input.names),
    is.null(id.col) || (length(id.col) == 1 && (id.col %in% (1 : input.ncol) || id.col %in% input.names)),
    is.null(block.cols) || all(block.cols %in% (1 : input.ncol)) || all(block.cols %in% input.names),
    is.null(coord.cols) || all(coord.cols %in% (1 : input.ncol)) || all(coord.cols %in% input.names),
    #family == "binomial",  # replaced with more explanatory message above
    test.sample >= 0 | test.sample == "Huberty",
    length(test.sample) == 1 | (is.integer(test.sample) & test.sample > 0),
    length(test.sample) < nrow(data),
    is.logical(FDR),
    is.logical(step),
    start %in% c("null.model", "full.model"),
    direction %in% c("both", "backward", "forward"),
    select %in% c("AIC", "BIC", "p.value"),
    is.logical(Y.prediction),
    is.logical(P.prediction),
    is.logical(Favourability),
    is.logical(group.preds),
    is.logical(trim),
    is.logical(TSA)
  )

  if (select == "p.value" && trim == TRUE) warning("When select='p.value', it is advisable to set trim=FALSE to avoid mixed significance criteria -- see help file for details.")

  data$sample <- "train"
  n <- nrow(data)  # [is.finite(data[ , sp.cols]), ]  # but this can differ among spp
  data.row <- 1:n

  # NEW:
  # n.init <- nrow(data)
  # data <- na.omit(data[ , c(sp.cols, var.cols)])
  # na.loss <- n.init - nrow(data)
  # if (na.loss > 0) message(na.loss, " rows excluded due to missing values.")

  # UNDER CONSTRUCTION:
  #if (test.sample != 0 && n != nrow(na.omit(data[ , c(sp.cols, var.cols, coord.cols)]))) warning("'data' include missing values, thus 'test.sample' may not always contain the expected amount of actual data.")  # new

  test.sample.input <- test.sample

  if (length(test.sample) == 1) {
    if (test.sample == "Huberty") {
      if (!FDR & !step & !trim) {
        test.sample <- percentTestData(length(var.cols)) / 100
        n.test <- round(n * test.sample)
        if (verbosity > 0)  message(
          "Following Huberty's rule, ", test.sample * 100, "% of observations
          (", n.test, " out of ", n, ") set aside for model testing; ",
          n - n.test, " observations used for model training.")
      } else stop ("Sorry, Huberty's rule cannot be used with 'FDR', 'step' or 'trim',
                   as these make the number of variables vary.
                   Set these 3 parameters to FALSE,
                   or use a different 'test.sample' option.")
    }  # end if Huberty
    else if (test.sample == 0) {
      if (verbosity > 0)  message("All ", n, " observations used for model training;
              none reserved for model testing.")
      n.test <- 0
    } else if (test.sample < 1) {
      n.test <- round(n * test.sample)
      if (verbosity > 0)  message(
        test.sample * 100, "% of observations (", n.test, " out of ", n, ") set aside for model testing; ",
        n - n.test, " observations used for model training.")
    } else if (test.sample >= 1) {
      n.test <- test.sample
      if (verbosity > 0)  message(
        n.test, " (out of ", n, ") observations set aside for model testing; ",
        n - n.test, " observations used for model training.")
    }
    test.sample <- sample(data.row, size = n.test, replace = FALSE)
  } else if (length(test.sample) > 1) {
    n.test <- length(test.sample)
    if (verbosity > 0)  message(
      n.test, " (out of ", n, ") observations set aside for model testing; ",
      n - n.test, " observations used for model training.")
  }

  data$sample[data.row %in% test.sample] <- "test"
  train.data <- data[data$sample == "train", ]

  if (Favourability) {
    if (family != "binomial") {
      Favourability <- FALSE
      warning ("Favourability not computed, as it is only applicable when the
                response variable is binary.")
    }  # end if family != binomial (for when other families are implemented)
  }  # end if Fav

  if (!is.numeric(sp.cols)) sp.cols <- which(colnames(data) %in% sp.cols)  # new
  if (!is.numeric(var.cols)) var.cols <- which(colnames(data) %in% var.cols)  # new
  if (!is.null(coord.cols) && !is.numeric(coord.cols)) coord.cols <- which(colnames(data) %in% coord.cols)  # new
  if (!is.null(id.col) && !is.numeric(id.col)) id.col <- which(colnames(data) %in% id.col)  # new
  if (!is.null(block.cols) && !is.numeric(block.cols)) block.cols <- which(colnames(data) %in% block.cols)  # new

  keeP <- P.prediction  # keep P only if the user wants it
  if (Favourability)  P.prediction <- TRUE  # P is necessary to calculate Fav
  n.models <- length(sp.cols)
  n.preds <- n.models * (Y.prediction + P.prediction + Favourability)  # sums logical values of function arguments
  models <- vector("list", n.models)
  predictions <- matrix(NA, nrow = nrow(data), ncol = n.preds)
  colnames(predictions) <- rep("", n.preds)
  model.count <- 0
  pred.count <- 0

  # UNDER CONSTRUCTION [MOVE TO sp.col LOOP]:
  # exclusions <- c(FDR, corSelect, step, trim)  # this block new
  # n_exclusions <- sum(exclusions)
  # if (n_exclusions > 0) {
  #   excluded <- vector("list", n_exclusions)
  #   excl_names <- c("FDR", "corSelect", "step", "trim")
  #   names(excluded) <- excl_names[which(exclusions == TRUE)]
  # }

  for (s in sp.cols) {
    model.count <- model.count + 1
    response <- colnames(train.data)[s]

    if (verbosity > 0)  cat("\n\n=> Building model ", model.count, " of ", n.models, " (", response, ")...\n\n", sep = "")
    if (verbosity > 1) {
      cat(length(var.cols) + length(block.cols), "input predictor variable(s)")
      if (!is.null(block.cols))  cat("[including", length(block.cols), "block variable(s)]")
      cat("\n\n")
    }

    if (TSA) {
      if (verbosity > 1)  cat("...", length(var.cols) + 1, "with the spatial trend variable\n\n")
      tsa <- suppressMessages(multTSA(data = data, sp.cols = s, coord.cols = coord.cols, degree = degree, type = "Y"))
      tsa_name <- paste("sptrend", response, sep = "_")
      data <- data.frame(data, tsa[ , ncol(tsa)])
      names(data)[ncol(data)] <- tsa_name
      train.data <- data.frame(train.data, tsa[which(data$sample == "train"), ncol(tsa)])
      names(train.data)[ncol(train.data)] <- tsa_name
      var.cols <- c(var.cols, ncol(train.data))
    }  # end if TSA

    #attach(train.data, warn.conflicts = FALSE)
    #on.exit(detach(train.data), add = TRUE)

    # UNDER CONSTRUCTION:
    #data_noNA <- na.omit(train.data[ , c(s, var.cols, id.col)])  # new
    #n_noNA <- nrow(data_noNA)
    #if (n_noNA != n) {
    #  cat(n - n_noNA, " rows omitted due to missing data for ", response)
    #}

    if (FDR && FDR.first) {
      fdr <- FDR(data = train.data, sp.cols = s, var.cols = var.cols, correction = correction, verbosity = 0)
      if (nrow(fdr$select) == 0) {
        message(paste0(
          "No variables passed the FDR test (so no variables included in the model)\n for '", response, "'. Consider using 'FDR = FALSE' or choosing a less stringent 'correction' procedure."))
        #next
      } #else {
      #excluded["FDR"] <- row.names(fdr$exclude)
      if (verbosity > 1)  cat(length(var.cols) - nrow(fdr$select), "variable(s) excluded by 'FDR' function\n", paste(row.names(fdr$exclude), collapse = ", "), "\n\n")
      #}
      sel.var.cols <- which(colnames(train.data) %in% rownames(fdr$select))
    }  # end if FDR & FDR.first
    else sel.var.cols <- var.cols

    if (length(sel.var.cols) > 1 && isTRUE(corSelect)) {
      corselect <- suppressMessages(corSelect(data = train.data, sp.cols = s, var.cols = sel.var.cols, coeff = coeff, cor.thresh = cor.thresh, use = "pairwise.complete.obs"))
      corsel.var.cols <- corselect$selected.var.cols
      if (verbosity > 1)  cat(length(sel.var.cols) - length(corsel.var.cols), "variable(s) excluded by 'corSelect' function\n", paste(corselect$excluded.vars, collapse = ", "), "\n\n")
      sel.var.cols <- corsel.var.cols
    }  # end if corSelect

    if (length(sel.var.cols) > 0 && isTRUE(FDR) && isFALSE(FDR.first)) {
      fdr <- FDR(data = train.data, sp.cols = s, var.cols = sel.var.cols, correction = correction, verbosity = 0)
      if (nrow(fdr$select) == 0) {
        message(paste0(
          "No variables passed the FDR test (so no variables included in the model)\n for '", response, "'. Consider using 'FDR = FALSE' or choosing a less stringent 'correction' procedure."))
        #next
      } #else {
      #excluded["FDR"] <- row.names(fdr$exclude)
      if (verbosity > 1)  cat(length(sel.var.cols) - nrow(fdr$select), "variable(s) excluded by 'FDR' function\n", paste(row.names(fdr$exclude), collapse = ", "), "\n\n")
      #}
      sel.var.cols <- which(colnames(train.data) %in% rownames(fdr$select))
    }  # end if FDR & !FDR.first

    if (length(sel.var.cols) == 0)  model.vars <- 1
    else  model.vars <- colnames(train.data)[sel.var.cols]
    model.formula <- with(train.data, as.formula(paste(response, "~", paste(model.vars, collapse = "+"))))

    model.expr <- expression(glm(model.formula, family = binomial))

    if (step && length(sel.var.cols) > 0) {

      n.vars.start <- length(sel.var.cols)
      # n.vars.start <- length(sel.var.cols) + length(block.cols)

      if (select == "p.value") {  # NEW
        model <- stepwise(data = train.data, sp.col = response, var.cols = model.vars, family = family, direction = direction, trace = trace, test.in = test.in, test.out = test.out, p.in = p.in, p.out = p.out)
      } else {

        if (select == "AIC") K <- 2
        else if (select == "BIC") K <- log(n)

        if (start == "full.model") {

          model <- step(eval(model.expr), direction = direction, trace = trace, k = K)
        } else if (start == "null.model") {
          model.scope <- model.formula[-2]  # removes response from formula
          null.formula <- as.formula(paste(response, "~", 1))
          model <- step(glm(null.formula, family = binomial, data = train.data),
                        direction = direction, scope = model.scope, trace = trace, k = K)
        } else stop ("'start' must be either 'full.model' or 'null.model'")
      }  # end if AIC or BIC

      n.vars.step <- length(model$coefficients) - 1
      excluded.vars <- setdiff(colnames(data[ , sel.var.cols]), names(model$coefficients)[-1])
      step_fun <- ifelse(select == "p.value", "stepwise", "step")
      if (verbosity > 1)  cat(n.vars.start - n.vars.step, " variable(s) excluded by '", step_fun, "' function\n", paste(excluded.vars, collapse = ", "), "\n\n", sep = "")
    } else model <- eval(model.expr, envir = train.data)

    if (trim && length(model$coefficients) > 1) {  # length(sel.var.cols)
      n.vars.start <- length(model$coefficients) - 1
      names.vars.start <- names(model$coefficients)[-1]
      # cat("+++ variables for modelTrim:\n", paste(names.vars.start, collapse = ", "), "\n\n")
      model <- suppressMessages(modelTrim(model, ...))
      # model <- stepwise(data = model$model, sp.col = 1, var.cols = 2:ncol(model$model), direction = "backward", Favourability = FALSE, simplif = TRUE, trace = 0, ...)
      n.vars.trim <- length(model$coefficients) - 1
      excluded.vars <- setdiff(names.vars.start, names(model$coefficients)[-1])
      if (verbosity > 1)  cat(n.vars.start - n.vars.trim, " variable(s) excluded by 'modelTrim' function\n ", paste(excluded.vars, collapse = ", "), "\n\n", sep = "")
    }

    if (step || trim) {
      sel.var.names <- names(model$coefficients)[-1]
      if (verbosity > 1)  cat(length(sel.var.names), "variable(s) INCLUDED IN THE FINAL MODEL\n",
                              paste(sel.var.names, collapse = ", "))
    }

    if (verbosity > 1)  cat("\n\n")

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

    if (TSA) {
      train.data <- train.data[ , -ncol(train.data)]
      data <- data[ , -ncol(data)]
      var.cols <- var.cols[-length(var.cols)]
    }  # end if TSA 2

    #detach(train.data)

  }  # end for s

  #if (rm.null.models) models <- models[!sapply(models, is.null)]

  if (P.prediction && !keeP) {
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

    if (n.pred.types == 1 || length(sp.cols) == 1)  group.preds <- FALSE

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
      colnames(predictions)[1] <- input.names[id.col]
    }

  }  # end if pred.types 0 else

  if (test.sample.input == 0) {
    predictions <- predictions[ , - match("sample", colnames(predictions))]
  }

  if (!is.null(id.col)) {
    colnames(predictions)[1] <- colnames(data)[id.col]
  } # new

  selected_variables <- lapply(models, function (x) names(x$model)[-1])

  message("Finished!")
  return(list(predictions = predictions, models = models, variables = selected_variables))

}  # end multGLM function
