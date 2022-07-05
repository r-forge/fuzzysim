multTSA <- function(data, sp.cols, coord.cols, id.col = NULL, degree = 3, step = TRUE, criterion = "AIC", type = "P", Favourability = FALSE, suffix = "_TS", save.models = FALSE, verbosity = 2, ...) {

  # version 2.8 (5 Jul 2022)

  if (verbosity > 1) {
    start.time <- Sys.time()
    on.exit(timer(start.time))
  }
  
  data <- as.data.frame(data)

  stopifnot (
    na.omit(as.matrix(data[ , sp.cols])) %in% c(0,1),
    #data[ , sp.cols] %in% c(NA, 0, 1),
    length(sp.cols) > 0,
    length(sp.cols) <= ncol(data) - length(coord.cols) - length(id.col),
    sp.cols %in% 1:ncol(data) | sp.cols %in% colnames(data),
    is.null(coord.cols) | length(coord.cols) == 2,
    is.null(coord.cols) | coord.cols %in% 1:ncol(data) | coord.cols %in% colnames(data),
    is.numeric(as.matrix(data[ , coord.cols])),
    is.null(id.col) | id.col %in% 1:ncol(data) | id.col %in% colnames(data),
    degree %% 1 == 0,
    is.logical(step),
    type %in% c("Y", "P", "F"),
    criterion %in% c("AIC", "significance"),  # new
    is.logical(Favourability),
    is.logical(save.models)
  )

  if (Favourability == TRUE) {
    warning("Argument 'Favourability' is deprecated; internally converted to type = 'F'.")
    type <- "F"
  }

  coords.poly <- as.data.frame(poly(as.matrix(data[ , coord.cols]),
                                    degree = degree, raw = TRUE, simple = TRUE))
  n.poly.terms <- ncol(coords.poly)

  if (is.character(coord.cols))  coord.names <- coord.cols
  else  coord.names <- colnames(data)[coord.cols]
  colnames(coords.poly) <- gsub(pattern = "\\.", replacement = "_", x = colnames(coords.poly))
  colnames(coords.poly) <- paste0(coord.names[1], colnames(coords.poly))
  colnames(coords.poly) <- gsub(pattern = "_", replacement = paste0("_", coord.names[2]), x = colnames(coords.poly))

  sp.data <- as.matrix(data[ , sp.cols])
  colnames(sp.data) <- colnames(data[ , sp.cols, drop = FALSE])
  n.subjects <- length(sp.cols)
  if (save.models) models <- vector("list", n.subjects)
  subj.count <- 0

  data.input <- data
  data <- cbind(data, coords.poly)

  for (s in 1:n.subjects) {
    subj.count <- subj.count + 1
    subj.name <- colnames(sp.data)[s]
    if (verbosity > 0) message("Computing TSA ", subj.count, " of ", n.subjects, " (", subj.name, ") ...")
    model.formula <- as.formula(paste(subj.name, "~", paste(colnames(coords.poly), collapse = "+")))
    model.expr <- expression(with(data, glm(model.formula, family = binomial, data = data)))
    if (step) {
      if (criterion == "AIC") model <- step(eval(model.expr), trace = 0)
      else if (criterion == "significance") model <- modelTrim(eval(model.expr), ...)
    }
    else model <- eval(model.expr)

    if (type == "Y")  tp = "link"
    else if (type == "P" | type == "F")  tp = "response"

    pred <- predict(model, coords.poly, type = tp)

    if (type == "F") {
      #n1 <- sum(sp.data[ , s] == 1)
      #n0 <- sum(sp.data[ , s] == 0)
      #pred <- (pred / (1 - pred)) / ((n1 / n0) + (pred / (1 - pred)))
      pred <- Fav(obs = sp.data[ , s], pred = pred)
    }
    data[ , ncol(data) + 1] <- pred
    colnames(data)[ncol(data)] <- paste0(subj.name, suffix)
    if (save.models) {
      models[[subj.count]] <- model
      names(models)[[subj.count]] <- subj.name
    }
  }

  predictions <- data.frame(data[ , id.col], data[ , ((ncol(data.input) + 1 + n.poly.terms) : ncol(data)), drop = FALSE])

  if (!is.null(id.col)) {
    if (is.character(id.col)) colnames(predictions)[1] <- id.col
    else colnames(predictions)[1] <- colnames(data)[id.col]
  }

  if (verbosity > 1)  message("Finished!")

  if (save.models) return(list(predictions = data.frame(predictions), models = models))
  else return (predictions)

}
