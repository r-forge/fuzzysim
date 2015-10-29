corSelect <- function(data, sp.cols, var.cols, cor.thresh = 0.8, select = "p.value", ...) {
  
  # version 1.2 (29 Oct 2015)
  
  data <- data[is.finite(data[ , sp.cols]), ]
  
  cor.mat <- cor(data[ , var.cols], ...)
  cor.mat[upper.tri(cor.mat, diag = TRUE)] <- NA
  
  if (max(abs(cor.mat), na.rm = TRUE) >= cor.thresh) {
    
    high.cor.mat <- as.matrix(which(abs(cor.mat) >= cor.thresh, arr.ind = TRUE))
    rownames(high.cor.mat) <- NULL
    high.cor.inds <- sort(unique(as.vector(high.cor.mat)))
    high.cor.mat <- data.frame(high.cor.mat, var1 = rownames(cor.mat)[high.cor.mat[ , "row"]], var2 = colnames(cor.mat)[high.cor.mat[ , "col"]])
    high.cor.mat$corr <- NULL
    for (r in 1:nrow(high.cor.mat))  high.cor.mat$corr[r] <- cor.mat[high.cor.mat$row[r], high.cor.mat$col[r]]
    
    high.cor.vars <- unique(rownames(cor.mat[high.cor.inds, high.cor.inds]))
    bivar.mat <- FDR(data = data, sp.cols = sp.cols, var.cols = match(high.cor.vars, colnames(data)), simplif = TRUE)[ , c("AIC", "p.value")]
    if (all(order(bivar.mat[ , c("AIC")]) == order(bivar.mat[ , c("p.value")])))  message("Results identical using whether p-value or AIC to select variables.\n")
    else message("Results NOT identical using whether p-value or AIC to select variables.\n")
    
    while (max(abs(cor.mat), na.rm = TRUE) >= cor.thresh) {
      max.cor.ind <- which(abs(cor.mat) == max(abs(cor.mat), na.rm = TRUE), arr.ind = TRUE)
      v1 <- rownames(cor.mat)[max.cor.ind[1]]
      v2 <- colnames(cor.mat)[max.cor.ind[2]]
      targets <- bivar.mat[c(v1, v2), select, drop = FALSE]
      exclude <- which.max(as.matrix(targets))
      excl.name <- rownames(targets)[exclude]
      excl.ind <- match(excl.name, colnames(cor.mat))
      cor.mat <- cor.mat[-excl.ind, -excl.ind, drop = FALSE]
      if (all(is.na(cor.mat))) cor.mat <- numeric(0)
      if (length(cor.mat) == 0) break
    }  # end while max
    
  }  # end if max > thresh
  else high.cor.mat <- bivar.mat <- numeric(0)
  
  selected.vars <- colnames(cor.mat)
  selected.var.cols <- match(colnames(cor.mat), colnames(data))
  excluded.vars <- colnames(data)[var.cols][!(colnames(data)[var.cols] %in% colnames(cor.mat))]
    
  vif <- multicol(data[ , selected.var.cols])
  
  list(high.correlations = high.cor.mat, 
       bivariate.significance = bivar.mat, 
       excluded.vars = excluded.vars,
       selected.vars = selected.vars,
       selected.var.cols = selected.var.cols,
       strongest.remaining.corr = cor.mat[which.max(abs(cor.mat))],
       remaining.multicollinearity = vif
  )
}  # end corSelect function
