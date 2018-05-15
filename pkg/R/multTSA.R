multTSA <-
function(data, sp.cols, coord.cols, id.col = NULL, degree = 3, step = TRUE,
         Favourability = FALSE, suffix = "_TS", save.models = FALSE) {

  # version 2.2 (5 May 2018)
  
  start.time <- Sys.time()

  stopifnot (
    na.omit(as.matrix(data[ , sp.cols])) %in% c(0,1),
    length(sp.cols) > 0,
    length(sp.cols) <= ncol(data) - length(coord.cols) - length(id.col),
    sp.cols %in% 1:ncol(data) | sp.cols %in% colnames(data),
    is.null(coord.cols) | length(coord.cols) == 2,
    is.null(coord.cols) | coord.cols %in% 1:ncol(data) | coord.cols %in% colnames(data),
    is.numeric(as.matrix(data[ , coord.cols])),
    is.null(id.col) | id.col %in% 1:ncol(data) | id.col %in% colnames(data),
    degree %% 1 == 0,
    is.logical(step),
    is.logical(Favourability),
    is.logical(save.models)
  )

  coords.poly <- as.data.frame(poly(as.matrix(data[ , coord.cols]),
                                    degree = degree, raw = TRUE))
  n.poly.terms <- ncol(coords.poly)
  colnames(coords.poly) <- paste0(rep("poly", n.poly.terms),
                                 c(1:n.poly.terms))

  sp.data <- as.matrix(data[ , sp.cols])
  colnames(sp.data) <- colnames(data[ , sp.cols, drop = FALSE])
  n.subjects <- length(sp.cols)
  if (save.models) TSA.models <- vector("list", n.subjects)
  subj.count <- 0

  data.input <- data
  data <- cbind(data, coords.poly)

  for (s in 1:n.subjects) {
    subj.count <- subj.count + 1
    subj.name <- colnames(sp.data)[s]
    message("Computing TSA ", subj.count, " of ", n.subjects, " (", subj.name, ") ...")
    model.formula <- as.formula(paste(subj.name, "~", paste(colnames(coords.poly), collapse = "+")))
    model.expr <- expression(with(data, glm(model.formula, family = binomial, data = data)))
    if (step)  model <- step(eval(model.expr), trace = 0)
    else model <- eval(model.expr)
    pred <- predict(model, coords.poly, type = "response")
    if (Favourability) {
      #n1 <- sum(sp.data[ , s] == 1)
      #n0 <- sum(sp.data[ , s] == 0)
      #pred <- (pred / (1 - pred)) / ((n1 / n0) + (pred / (1 - pred)))
      pred <- Fav(obs = sp.data[ , s], pred = pred)
    }
    data[ , ncol(data) + 1] <- pred
    colnames(data)[ncol(data)] <- paste0(subj.name, suffix)
    if (save.models) {
      TSA.models[[subj.count]] <- model
      names(TSA.models)[[subj.count]] <- subj.name
    }
  }

  predictions <- data.frame(data[ , id.col], data[ , ((ncol(data.input) + 1 + n.poly.terms) : ncol(data)), drop = FALSE])

  if (!is.null(id.col)) {
    if (is.character(id.col)) colnames(predictions)[1] <- id.col
    else colnames(predictions)[1] <- colnames(data)[id.col]
  }

  message("Finished!")
  timer(start.time)

  if (save.models) return(list(predictions = data.frame(predictions), TSA.models = TSA.models))
  else return (predictions)

}
