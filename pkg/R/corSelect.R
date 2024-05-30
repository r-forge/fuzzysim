corSelect <- function(data, sp.cols = NULL, var.cols, coeff = TRUE, cor.thresh = ifelse(isTRUE(coeff), 0.8, 0.05), select = ifelse(is.null(sp.cols), "VIF", "p.value"), test = "Chisq", family = "auto", use = "pairwise.complete.obs", method = "pearson", verbosity = 1) {

  # version 3.6 (13 Mar 2024)

  if (length(sp.cols) > 1) stop ("Sorry, 'corSelect' is currently implemented for only one 'sp.col' at a time.")

  univar.criteria <- c("VIF")
  bivar.criteria <- c("p.value", "AIC", "BIC", "cor")

  if (!is.null(select) && !(select %in% c(univar.criteria, bivar.criteria))) stop ("Invalid 'select' criterion.")

  data <- as.data.frame(data)  # accepts tibbles

  if (is.numeric(var.cols)) var.cols <- colnames(data)[var.cols]

  notnum <- names(which(!sapply(data[ , var.cols, drop = FALSE], is.numeric)))
  if (length(notnum) > 0) {
    warning("Disregarded the following non-numeric input variable(s):\n- ", paste(notnum, collapse = "\n- "), "\nYou can add them manually and consider finding a package with a correlation method appropriate for that type of variable(s).\n")
    if (is.numeric(var.cols)) var.cols <- names(data)[var.cols]
    var.cols <- var.cols[-grep(paste(notnum, collapse = "|"), var.cols)]
  }

  if (!is.null(sp.cols) && !is.null(select) && select %in% bivar.criteria) {
    n.in <- nrow(data)
    data <- data[is.finite(data[ , sp.cols]), ]
    n.out <- nrow(data)
    if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data in 'sp.cols'; ", n.out, " observations actually evaluated.")
  }

  cor.mat <- cor(data[ , var.cols], use = use, method = method)
  cor.mat[upper.tri(cor.mat, diag = TRUE)] <- NA

  if (isFALSE(coeff)) {
    cor.test.p <- function(x, method){ # https://stackoverflow.com/a/13112337
      FUN <- function(x, y) cor.test(x, y, method = method)[["p.value"]]
      z <- outer(
        colnames(x),
        colnames(x),
        Vectorize(function(i,j) FUN(x[,i], x[,j]))
      )
      dimnames(z) <- list(colnames(x), colnames(x))
      z
    }  # end cor.test.p function

    cor.mat.p <- cor.test.p(data[ , var.cols], method = method)
    cor.mat.p[upper.tri(cor.mat.p, diag = TRUE)] <- NA
  }

  if (isTRUE(coeff))  high.corrs.exist <- max(abs(cor.mat), na.rm = TRUE) >= cor.thresh
  if (isFALSE(coeff))  high.corrs.exist <- min(cor.mat.p, na.rm = TRUE) < cor.thresh

  high.cor.mat <- bivar.mat <- numeric(0)

  if (high.corrs.exist) {
    if (isTRUE(coeff)) high.cor.rowcol <- as.matrix(which(abs(cor.mat) >= cor.thresh, arr.ind = TRUE))
    if (isFALSE(coeff)) high.cor.rowcol <- as.matrix(which(cor.mat.p < cor.thresh, arr.ind = TRUE))
    high.cor.inds <- sort(unique(as.vector(high.cor.rowcol)))
    high.cor.mat <- data.frame(var1 = rownames(cor.mat)[high.cor.rowcol[ , "row"]], var2 = colnames(cor.mat)[high.cor.rowcol[ , "col"]])
    high.cor.mat$corr <- NA_real_
    high.cor.mat$p.value <- NA_real_
    for (r in 1:nrow(high.cor.mat)) {
      v1 <- high.cor.mat[r, "var1"]
      v2 <- high.cor.mat[r, "var2"]
      high.cor.mat$corr[r] <- cor.mat[v1, v2]
      if (isTRUE(coeff)) high.cor.mat$p.value[r] <- cor.test(data[ , v1], data[ , v2], method = method)$p.value  # checked against next line with all.equal() TRUE; this line needed if coeff=TRUE, as cor.mat.p is not created
      if (isFALSE(coeff)) high.cor.mat$p.value[r] <- cor.mat.p[v1, v2]
    }

    if (isTRUE(coeff)) high.cor.mat <- high.cor.mat[order(abs(high.cor.mat$corr), decreasing = TRUE), ]
    if (isFALSE(coeff)) high.cor.mat <- high.cor.mat[order(high.cor.mat$p.value, decreasing = FALSE), ]

    if (is.null(sp.cols) && !is.null(select) && select %in% bivar.criteria) {
      message("'select' criterion (", select, ") changed to NULL, as it cannot be assessed without a response variable ('sp.cols'). You can either specify 'sp.cols', or change 'select' to 'VIF'.")
      select <- NULL
    }

    if (is.null(select))  return (high.cor.mat)

    message("Using '", select, "' as the 'select' criterion.")

    high.cor.vars <- unique(rownames(cor.mat[high.cor.inds, high.cor.inds]))

    if (select %in% bivar.criteria) {
      bivar.mat <- FDR(data = data, sp.cols = sp.cols, var.cols = match(high.cor.vars, colnames(data)), family = family, simplif = TRUE, verbosity = 0)[ , c("p.value", "AIC", "BIC")]
      cors <- cor(data.frame(data[ , sp.cols, drop = FALSE], data[ , match(high.cor.vars, colnames(data)), drop = FALSE]), use = "pairwise.complete.obs")[-1, 1]
      bivar.mat <- data.frame(bivar.mat, cor = cors[rownames(bivar.mat)])  # 'rownames' puts them in same order
      # if (isTRUE(all.equal(order(bivar.mat[ , c("p.value")]), order(bivar.mat[ , c("AIC")]), order(bivar.mat[ , c("BIC")]), order(abs(bivar.mat[ , c("cor")])), tolerance = 1.5e-8)))  # wrong use of 'all.equal'
      if (isTRUE(all(sapply(list(order(bivar.mat[ , c("AIC")]), order(bivar.mat[ , c("BIC")]), order(abs(bivar.mat[ , c("cor")]))), FUN = identical, order(bivar.mat[ , c("p.value")])))))  # https://stackoverflow.com/a/30850654/3447652
        message("Results identical whether 'select' is 'p.value', 'AIC', 'BIC' or cor.\n") else message("Results NOT identical whether 'select' is 'p.value', 'AIC', 'BIC' or 'cor'.\n")
    }  # end if select in bivar

    data.remaining <- data[ , var.cols]

    if (isTRUE(coeff)) {
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

        if (select == "cor")  exclude <- which.min(as.matrix(abs(targets)))
        else exclude <- which.max(as.matrix(targets))
        excl.name <- rownames(targets)[exclude]
        excl.ind <- match(excl.name, colnames(cor.mat))
        data.remaining <- data.remaining[ , -excl.ind, drop = FALSE]
        cor.mat <- cor.mat[-excl.ind, -excl.ind, drop = FALSE]
        if (all(is.na(cor.mat))) cor.mat <- numeric(0)
        if (length(cor.mat) == 0) break
      }  # end while max
    }  # end TRUE coeff

    if (isFALSE(coeff)) {
      while (min(cor.mat.p, na.rm = TRUE) < cor.thresh) {
        min.p.ind <- which(cor.mat.p == min(cor.mat.p, na.rm = TRUE), arr.ind = TRUE)[1, ]  # [1, ] in case there are ties
        v1 <- rownames(cor.mat.p)[min.p.ind["row"]]
        v2 <- colnames(cor.mat.p)[min.p.ind["col"]]
        if (select %in% univar.criteria) {
          vif <- multicol(data.remaining)
          targets <- vif[c(v1, v2), "VIF", drop = FALSE]
        }  # end if univar
        else if (select %in% bivar.criteria) {
          targets <- bivar.mat[c(v1, v2), select, drop = FALSE]
        }  # end if bivar

        exclude <- which.max(as.matrix(targets))
        excl.name <- rownames(targets)[exclude]
        excl.ind <- match(excl.name, colnames(cor.mat.p))
        data.remaining <- data.remaining[ , -excl.ind, drop = FALSE]
        cor.mat.p <- cor.mat.p[-excl.ind, -excl.ind, drop = FALSE]
        if (all(is.na(cor.mat.p))) cor.mat.p <- numeric(0)
        if (length(cor.mat.p) == 0) break
      }  # end while max
    }  # end FALSE coeff

  }  # end if high.corrs

  if (isTRUE(coeff)) {
    selected.vars <- colnames(cor.mat)
    selected.var.cols <- match(colnames(cor.mat), colnames(data))
  }

  if (isFALSE(coeff)) {
    selected.vars <- colnames(cor.mat.p)
    selected.var.cols <- match(colnames(cor.mat.p), colnames(data))
  }

  if (is.character(var.cols)) var.cols <- which(colnames(data) %in% var.cols)  # new

  if (isTRUE(coeff)) excluded.vars <- colnames(data)[var.cols][!(colnames(data)[var.cols] %in% colnames(cor.mat))]
  if (isFALSE(coeff)) excluded.vars <- colnames(data)[var.cols][!(colnames(data)[var.cols] %in% colnames(cor.mat.p))]

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

  if (isFALSE(coeff)) cor.mat <- cor.mat[rownames(cor.mat.p), colnames(cor.mat.p)]
  strongest.remaining.corr <- suppressMessages(cor.mat[which.max(abs(cor.mat))])


  list(high.correlations = high.cor.mat,
       bivariate.significance = bivar.mat,
       excluded.vars = excluded.vars,
       selected.vars = selected.vars,
       selected.var.cols = selected.var.cols,
       strongest.remaining.corr = strongest.remaining.corr,
       remaining.multicollinearity = vif2
  )
}  # end corSelect function
