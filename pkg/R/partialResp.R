partialResp <- function(model, vars = NULL, Fav = FALSE, se.mult = 1.96, plot.points = FALSE, ylim = c(0, 1), reset.par = TRUE, ...) {

  if (!inherits(model, "glm") || !all(c("binomial", "logit") %in% model$family)) stop ("'model' must be of class 'glm' with 'binomial' family and 'logit' link.")

  vars_mod <- attr(stats::terms(model), "term.labels")

  if (is.null(vars)) {
    vars <- vars_mod
  } else {
    stopifnot(all(vars %in% vars_mod))
  }

  if (reset.par) {
    opar <- par(no.readonly = TRUE)
    par(mfrow = modEvA::arrangePlots(length(vars)), mar = c(3, 3, 1, 1))
    on.exit(par(opar))
  }

  dat <- model$model

  for (v in vars) {
    # create dataframe for partial prediction:
    range_v <- range(dat[ , v], na.rm = TRUE)
    vals_v <- seq(range_v[1], range_v[2], by = diff(range_v) / 100)
    # vals_v <- seq(floor(range_v[1]), ceiling(range_v[2]), by = diff(range_v) / 100)

    if (length(vars_mod) > 1)
      partial_df <- data.frame(vals_v, sapply(setdiff(vars_mod, v), function(x) rep(mean(dat[[x]], na.rm = TRUE), length(vals_v))))
    else
      partial_df <- data.frame(vals_v)
    names(partial_df)[1] <- v

    # compute the partial prediction and CIs (https://www.r-bloggers.com/2018/12/confidence-intervals-for-glms):
    pred_v <- predict(model, newdata = partial_df, type = "link", se.fit = TRUE)
    ilink <- model$family$linkinv
    prob_v <- ilink(pred_v$fit)
    # confidence intervals:
    upper_v <- ilink(pred_v$fit + (se.mult * pred_v$se.fit))
    lower_v <- ilink(pred_v$fit - (se.mult * pred_v$se.fit))

    if (!Fav) {
      xlim <- range(vals_v, na.rm = TRUE)
      if (plot.points) xlim <- range(xlim, dat[ , v], na.rm = TRUE)

      if (length(ylim) == 1 && ylim == "auto") {
        ylim <- range(c(lower_v, upper_v), na.rm = TRUE)
        if (plot.points)  ylim <- range(c(ylim, model$fitted.values), na.rm = TRUE)
      }

      plot(xlim, ylim, type = "n", xlab = v, ylab = "Probability", mgp = c(1.8, 0.6, 0), ...)
      polygon(x = c(vals_v, rev(vals_v)),
              y = c(upper_v, rev(lower_v)),  # confidence interval
              col = "#D3DDE9", border = NA)
      if (plot.points) points(dat[ , v], model$fitted.values, col = grDevices::rgb(0.7, 0.7, 0.7, alpha = 0.4), cex = 0.1)
      lines(vals_v, prob_v, lwd = 1.5, col = "darkblue")  # response curve

    } else {  # if Fav
      prev <- prevalence(model = model)
      fav_v <- Fav(pred = prob_v, sample.preval = prev)
      upper_fav_v <- Fav(pred = upper_v, sample.preval = prev)
      lower_fav_v <- Fav(pred = lower_v, sample.preval = prev)
      if (plot.points) fav <- Fav(pred = model$fitted.values, sample.preval = prev)

      xlim <- range(vals_v, na.rm = TRUE)
      if (plot.points) xlim <- range(xlim, dat[ , v], na.rm = TRUE)

      if (length(ylim) == 1 && ylim == "auto") {
        ylim <- range(c(lower_fav_v, upper_fav_v), na.rm = TRUE)
        if (plot.points)  ylim <- range(c(ylim, fav), na.rm = TRUE)
      }

        plot(xlim, ylim, type = "n", xlab = v, ylab = "Favourability", mgp = c(1.8, 0.6, 0), ...)
        polygon(x = c(vals_v, rev(vals_v)),
                y = c(upper_fav_v, rev(lower_fav_v)),  # confidence interval
                col = "#D3DDE9", border = NA)
        if (plot.points) points(dat[ , v], fav, col = grDevices::rgb(0.7, 0.7, 0.7, alpha = 0.4), cex = 0.1)
        lines(vals_v, fav_v, lwd = 1.5, col = "darkblue")  # response curve
    }  # end if Fav else
  }  # end for v
}
