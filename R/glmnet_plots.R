#' Prepare the data for ggplot of glmnet
#' @export
glmnet_ggplot_prepare <- function(model.lasso){
  coeffs <- model.lasso$beta %>% as.matrix() %>% t()
  coeffs.df <- as.data.frame(coeffs)
  coeffs.ext.df <- cbind(
    coeffs.df,
    L1norm = apply(abs(coeffs), 1, sum),
    dev.ratio = model.lasso$dev.ratio,
    log.lambda = log(model.lasso$lambda),
    rowid = 1:nrow(coeffs.df)
  )
  coeffs.ext.df.x <- coeffs.ext.df %>%
    dplyr::select(rowid, L1norm, dev.ratio, log.lambda)

  coeffs.ext.wide.df <- tidyr::gather(
    data = coeffs.ext.df %>% dplyr::select(-L1norm, -dev.ratio, -log.lambda),
    key = "group",
    value = "yval",
    -rowid
  ) %>%
    dplyr::left_join(
      coeffs.ext.df.x,
      by = "rowid"
    )
  coeffs.ext.wide.df
}



#' glmnet::plot.glmnet
#' @export
glmnet_plot_glmnet <- function(x, xvar = c("norm", "lambda", "dev"), label = FALSE, ...){
  xvar = match.arg(xvar)
  glmnet_plotCoef(x$beta, lambda = x$lambda, df = x$df, dev = x$dev.ratio,
                  label = label, xvar = xvar, ...)
}

#' @export
glmnet_plotCoef <- function(
  beta,  norm, lambda, df, dev, label = FALSE,
  xvar = c("norm", "lambda", "dev"),
  xlab = NULL,
  ylab = "Coefficients", ...) {
  which.nonzero = glmnet::nonzeroCoef(beta)
  nwhich.nonzero = length(which.nonzero)
  switch(nwhich.nonzero + 1,
         `0` = {
           warning("No plot produced since all coefficients zero")
           return()
         },
         `1` = warning("1 or less nonzero coefficients; glmnet plot is not meaningful")
  )
  beta = as.matrix(beta[which.nonzero, , drop = FALSE])
  xvar = match.arg(xvar)
  switch(xvar,
         norm = {
           index = if (missing(norm)){
             apply(abs(beta), 2, sum)
           } else {
             norm
           }
           iname = "L1 Norm"
           approx.f = 1
         },
         lambda = {
           index = log(lambda)
           iname = "Log Lambda"
           approx.f = 0
         },
         dev = {
           index = dev
           iname = "Fraction Deviance Explained"
           approx.f = 1
         })
  if (is.null(xlab)){
    xlab <- iname
  }
  dotlist <- list(...)
  type <- dotlist$type
  if (is.null(type)){
    graphics::matplot(
      index, t(beta),
      lty = 1,
      xlab = xlab, ylab = ylab, type = "l", ...
    )
  } else {
    graphics::matplot(
      index, t(beta),
      lty = 1,
      xlab = xlab, ylab = ylab, ...)
  }
  ## degrees of freedom (upper horizontal axis)
  atdf <- base::pretty(index)
  prettydf = stats::approx(
    x = index,
    y = df,
    xout = atdf,
    rule = 2,
    method = "constant",
    f = approx.f
  )$y
  graphics::axis(
    side = 3, # above
    at = atdf,
    labels = prettydf,
    tcl = NA
  )
  if (label) {
    nnz = length(which.nonzero)
    xpos = max(index)
    pos = 4
    if (xvar == "lambda") {
      xpos = min(index)
      pos = 2
    }
    xpos = rep(xpos, nnz)
    ypos = beta[, ncol(beta)]
    text(xpos, ypos, paste(which.nonzero), cex = 0.5, pos = pos)
  }

}
