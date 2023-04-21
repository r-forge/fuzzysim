corSelect <- function(data, sp.cols = NULL, var.cols, cor.thresh = 0.8, select = ifelse(is.null(sp.cols), "VIF", "p.value"), family = "auto", use = "pairwise.complete.obs", method = "pearson", verbosity = 1) {

  # version 3.3 (21 Apr 2023)

  if (length(sp.cols) > 1) stop ("Sorry, 'corSelect' is currently implemented for only one 'sp.col' at a time.")

  univar.criteria <- c("VIF")
  bivar.criteria <- c("p.value", "AIC", "BIC")

  if (!is.null(select) && !(select %in% c(univar.criteria, bivar.criteria))) stop ("Invalid 'select' criterion.")

  data <- as.data.frame(data)

  notnum <- names(which(!sapply(data[ , var.cols, drop = FALSE], is.numeric)))
  if (length(notnum) > 0) {
    warning("Non-numeric variable(s) not considered: ", paste(notnum, collapse = ", "), ". You can add them manually and consider finding a package with a correlation method appropriate for that type of variable.")
    if(is.numeric(var.cols)) var.cols <- names(data)[var.cols]
    var.cols <- var.cols[-grep(notnum, var.cols)]
  }

  if (!is.null(sp.cols) && !is.null(select) && select %in% bivar.criteria) {
    n.in <- nrow(data)
    data <- data[is.finite(data[ , sp.cols]), ]
    n.out <- nrow(data)
    if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data in 'sp.cols'; ", n.out, " observations actually evaluated.")
  }

  cor.mat <- cor(data[ , var.cols], use = use, method = method)
  cor.mat[upper.tri(cor.mat, diag = TRUE)] <- NA
  high.cor.mat <- bivar.mat <- numeric(0)

  if (max(abs(cor.mat), na.rm = TRUE) >= cor.thresh) {
    high.cor.rowcol <- as.matrix(which(abs(cor.mat) >= cor.thresh, arr.ind = TRUE))
    high.cor.inds <- sort(unique(as.vector(high.cor.rowcol)))
    high.cor.mat <- data.frame(var1 = rownames(cor.mat)[high.cor.rowcol[ , "row"]], var2 = colnames(cor.mat)[high.cor.rowcol[ , "col"]])
    high.cor.mat$corr <- NULL
    for (r in 1:nrow(high.cor.mat))  high.cor.mat$corr[r] <- cor.mat[high.cor.rowcol[ ,"row"][r], high.cor.rowcol[ ,"col"][r]]
    high.cor.mat <- high.cor.mat[order(abs(high.cor.mat$corr), decreasing = TRUE), ]

    if (is.null(sp.cols) && !is.null(select) && select %in% bivar.criteria) {
      message(select, " 'select' criterion changed to NULL, as it is not assessable without a response variable ('sp.cols'). You can either specify 'sp.cols', or change 'select' to 'VIF'.")
      select <- NULL
    }

    if (is.null(select))  return (high.cor.mat)

    message("Using ", select, " as the 'select' criterion.")

    high.cor.vars <- unique(rownames(cor.mat[high.cor.inds, high.cor.inds]))

    if (select %in% bivar.criteria) {
      bivar.mat <- FDR(data = data, sp.cols = sp.cols, var.cols = match(high.cor.vars, colnames(data)), family = family, simplif = TRUE, verbosity = 0)[ , c("p.value", "AIC", "BIC")]
      if (isTRUE(all.equal(order(bivar.mat[ , c("p.value")]), order(bivar.mat[ , c("AIC")]), order(bivar.mat[ , c("BIC")]), tolerance = 1.5e-8)))  message("Results identical whether 'select' is p-value, AIC or BIC.\n") else message("Results NOT identical whether 'select' is p-value, AIC or BIC.\n")
    }  # end if select in bivar

    data.remaining <- data[ , var.cols]
    while (max(abs(cor.mat), na.rm = TRUE) >= cor.thresh) {
      max.cor.ind <- which(abs(cor.mat) == max(abs(cor.mat), na.rm = TRUE), arr.ind = TRUE)
      v1 <- rownames(cor.mat)[max.cor.ind[1]]
      v2 <- colnames(cor.mat)[max.cor.ind[2]]
      if (select %in% univar.criteria) {
        vif <- multicol(data.remaining)
        targets <- vif[c(v1, v2), "VIF", drop = FALSE]
      }  # end if univar
      else if (select %in% bivar.criteria) {
        targets <- bivar.mat[c(v1, v2), select, drop = FALSE]
      }  # end if bivar

      exclude <- which.max(as.matrix(targets))
      excl.name <- rownames(targets)[exclude]
      excl.ind <- match(excl.name, colnames(cor.mat))
      data.remaining <- data.remaining[ , -excl.ind, drop = FALSE]
      cor.mat <- cor.mat[-excl.ind, -excl.ind, drop = FALSE]
      if (all(is.na(cor.mat))) cor.mat <- numeric(0)
      if (length(cor.mat) == 0) break
    }  # end while max

  }  # end if max > thresh

  selected.vars <- colnames(cor.mat)
  selected.var.cols <- match(colnames(cor.mat), colnames(data))
  if (is.character(var.cols)) var.cols <- which(colnames(data) %in% var.cols)  # new
  excluded.vars <- colnames(data)[var.cols][!(colnames(data)[var.cols] %in% colnames(cor.mat))]

  vif2 <- multicol(data[ , selected.var.cols, drop = FALSE])

  if (verbosity > 0) {
    message(length(excluded.vars), " variable(s) excluded, ",
            length(selected.vars), " selected")
  }

  if (verbosity > 1) {
    cat("\nEXCLUDED:\n")
    cat(excluded.vars, sep = ", ")
    cat("\n\nSELECTED:\n")
    cat(selected.vars, sep = ", ")
    cat("\n")
  }

  list(high.correlations = high.cor.mat,
       bivariate.significance = bivar.mat,
       excluded.vars = excluded.vars,
       selected.vars = selected.vars,
       selected.var.cols = selected.var.cols,
       strongest.remaining.corr = cor.mat[which.max(abs(cor.mat))],
       remaining.multicollinearity = vif2
  )
}  # end corSelect function
