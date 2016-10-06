#' The glmnet model postprocessor
#'
#' The function is based on the results returned by \code{\link[broom]{glmnet_tidiers}},
#' with an optional application of results filtering using the \code{\link{structural.changes.col.matrix}} function.
#'
#' @param glmnet.res the result of the \code{\link[glmnet]{glmnet}} function
#' @param nonzero.coeffs.only if \code{TRUE} only the non-zero model coefficients are returned in the \code{coeffs} field
#' @param structural.filter.type the type of the \code{steps} filter, selecting the steps from the \emph{lambda path} resulting in the change of the model structure:
#'  \itemize{
#'    \item{(\code{"none"}): all steps are returned (default)}
#'    \item{(\code{"initial"}): return initial step of each subpath consisting of the same variables}
#'    \item{(\code{"final"}): return final step of each subpath consisting of the same variables}
#'  }
#' @param pseudoAIC \code{(logical)} should AIC be calculated for each model;
#'   Remark: the value being calculated is \code{2*df -2*loglike + 2*loglike(NULL)}, where \code{df} is an approximation of df returned by glmnet
#'
#' @return the \code{list} with following fields:
#'  \itemize{
#'    \item{\code{steps}: a \code{data.frame} with columns:
#'      \itemize{
#'        \item{\code{step}}
#'        \item{\code{lambda}}
#'        \item{\code{dev.ratio}}
#'        \item{\code{df}}
#'        \item{\code{dev}}
#'        \item{(optionally) \code{pseudoAIC}}
#'      }
#'    }
#'    \item{\code{coeffs}: a \code{data.frame} with columns:
#'      \itemize{
#'        \item{\code{step}}
#'        \item{\code{term}}
#'        \item{\code{estimate}}
#'      }
#'    }
#'  }
#' @export
glmnet.process.output <- function(glmnet.res, nonzero.coeffs.only = FALSE, structural.filter.type = c("none", "initial", "final"), pseudoAIC = FALSE){

  glmnet.out <- broom::tidy(glmnet.res)
  glmnet.res.dev <- glmnet::deviance.glmnet(glmnet.res)

  glmnet.out.steps <- glmnet.out %>%
    dplyr::select(step, lambda, dev.ratio) %>%
    dplyr::distinct() %>%
    dplyr::arrange(step)

  glmnet.out.steps$df <- glmnet.res$df
  glmnet.out.steps$dev <- glmnet.res.dev
  if (pseudoAIC) {
    null.deviance <- glmnet.res$nulldev
    # dev == 2*(loglike_sat - loglike)
    # null.deviance = 2*(loglike_sat - loglike(NULL model)
    # a "NULL model", is a model with icept only (or "empty model" - coxph)
    # AIC = 2*no.params - 2*ln(Like)
    # dev - null.deviance ==
    #   == 2*(loglike_sat - loglike) - 2*(loglike_sat - loglike(NULL) ==
    #   == -2*loglike + 2*loglike(NULL)
    # pseudoAIC = 2*df + (dev - null.deviance) ==
    #   == 2*no.params -2*loglike + 2*loglike(NULL) == AIC + const
    glmnet.out.steps <- glmnet.out.steps %>%
      dplyr::mutate(
        pseudoAIC = 2*df + (dev - null.deviance)
        )
  }

  glmnet.out.coeffs <- glmnet.out %>%
    dplyr::select(step, term, estimate)

  if (nonzero.coeffs.only) {
    glmnet.out.coeffs <- glmnet.out.coeffs %>%
      dplyr::filter(estimate != 0)
  }

  if (structural.filter.type[1] != "none") {
    coeff.mtx <- stats::coef(glmnet.res)

    changes.idx <- structural.changes.col.matrix(
      mtx = coeff.mtx,
      initial.substeps = ("initial" %in% structural.filter.type),
      final.substeps = ("final" %in% structural.filter.type)
    )
    glmnet.out.steps <- glmnet.out.steps %>% dplyr::filter(step %in% changes.idx)
    glmnet.out.coeffs <- glmnet.out.coeffs %>% dplyr::filter(step %in% changes.idx)

  }

  list(
    steps = glmnet.out.steps,
    coeffs = glmnet.out.coeffs
  )
}


#' Extraction of critical steps from Lasso path
#'
#' Function extracts the steps of the LASSO path resulting in the structural changes of
#' the model coefficients matrix (adding/dropping coefficient)
#' as well as the preceeding ones
#'
#' @param mtx the coefficients
#' @param strict if \code{TRUE} only the points of the structural change will be returned
#' @return \code{(numeric)} vector containing the steps with structural changes
#' @export
structural.changes.col.matrix <- function(mtx, initial.substeps = TRUE, final.substeps = TRUE) {
  if (ncol(mtx) == 1) {
    return(c(1))
  }

  # getting the inclusion/exclusion matrix
  coeff.incl <- (mtx != 0)

  # first coeff set - s0 - is always novel, we compare starting with s1
  coeff.incl.2 <- coeff.incl[, -1]

  # dimensions must match, we do remove the last column
  coeff.incl.3 <- coeff.incl[, -ncol(coeff.incl)]

  coeff.incl.diff <- (coeff.incl.2 != coeff.incl.3)
  coeff.incl.diff.reduced <- apply(coeff.incl.diff, 2, any)
  # coeff.incl.diff.reduced[i] == FALSE <-> coeffs[i+1] == coeffs[i]
  # coeff.incl.diff.reduced[i] == TRUE <-> coeffs[i+1] != coeffs[i]
  #   that is, coeffs[i+1] are DIFFERENT   (***)

  coeff.incl.diff.prv <- which(coeff.incl.diff.reduced)
  # all steps with novelties

  # adding the s0, adding (+1) to account for the (***)
  subpaths.initial.elems <- c(1, 1 + coeff.incl.diff.prv)
  subpaths.final.elems <- c(coeff.incl.diff.prv, ncol(coeff.incl))

  res <- c()

  if (initial.substeps) {
    res <- subpaths.initial.elems
  }
  if (final.substeps) {
    res <- c(res, subpaths.final.elems)
  }
  if (initial.substeps && final.substeps) {
    # removing duplicates, which may occur
    res <- sort(unique(res))
  }

  res
}

#' Extraction of critical points from Lasso path
#'
#' Function extracts the points of the structure change (adding/dropping coefficient)
#' as well as the preceeding ones, along with their deviance
#'
#' @param model.full.lasso the model fitted by \code{\link[glmnet]{glmnet}} function
#' @param strict if \code{TRUE} only the points of the structural change will be returned
#' @export
inference.lasso.path.structural.changes <- function(model.full.lasso, strict = FALSE){
  # extracting pts of structural change of the model
  # by comparing inclusion/exclusion matrices

  coeff.mtx <- stats::coef(model.full.lasso)
  changes.idx <- structural.changes.col.matrix(
    mtx = coeff.mtx,
    initial.substeps = TRUE,
    final.substeps = strict
    )

  lasso.struct.coeffs <- coeff.mtx[, changes.idx]
  Deviance <- stats::deviance(model.full.lasso)[changes.idx]
  lasso.struct <- rbind(lasso.struct.coeffs, Deviance)
  lasso.struct
}

#' @export
glmnet.predict.models <- function(glmnet.res, x, y, s, offset = NULL, exact = FALSE, ...) {

  # since the update() of the glmnet not always works, let's expect it won't and do the job ;
  # from the glmnet package:
  if (exact && (!is.null(s))) {
    which.s.computed <- match(s, glmnet.res$lambda, FALSE)
    if (!all(which.s.computed > 0)) {
      lambda <- unique(rev(sort(c(s, glmnet.res$lambda))))
      glmnet.res <- glmnet::glmnet(x = x, y = y, lambda = lambda, offset = offset, ...)
      # now all predict should work!
    }
  }

  pred.values <- predict(
    glmnet.res,
    type = "response",
    newx = x,
    offset = offset,
    exact = exact,
    s = s
  )

  if (is.matrix(y)) {
    mat.y.ok <- y
  } else {
    mat.y.ok <- matrix(y, ncol = 1)
  }

  bern.dev.1 <- t(log(pred.values)) %*% mat.y.ok
  bern.dev.2 <- t(log(1 - pred.values)) %*% (1 - mat.y.ok)
  model.llik <- (bern.dev.1 + bern.dev.2)

  pred.coeffs <- predict(glmnet.res, type = "coefficients", s = s, exact = exact)

  models.defs <- data.frame(s = s, loglik = model.llik[, 1])
  models.defs$model.id <- 1:nrow(models.defs)

  models.defs$df <- apply(pred.coeffs, 2, function(x) sum(x != 0))

  models.coeffs <- broom::tidy(pred.coeffs) %>%
    dplyr::rename(
      term = row,
      model.id = column,
      estimate = value
    )

  models.defs2 <- models.defs %>%
    dplyr::mutate(AIC = 2 * df - loglik)


  list(
    models = models.defs2,
    coeffs = models.coeffs
  )
}




