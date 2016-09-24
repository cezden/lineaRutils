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
glmnet.process.output <- function(glmnet.res, nonzero.coeffs.only = FALSE, structural.filter.type = c("none", "initial", "final")){

  glmnet.out <- broom::tidy(glmnet.res)
  glmnet.res.dev <- glmnet::deviance.glmnet(glmnet.res)

  glmnet.out.steps <- glmnet.out %>%
    dplyr::select(step, lambda, dev.ratio) %>%
    dplyr::distinct() %>%
    dplyr::arrange(step)

  glmnet.out.steps$df <- glmnet.res$df
  glmnet.out.steps$dev <- glmnet.res.dev

  glmnet.out.coeffs <- glmnet.out %>%
    dplyr::select(step, term, estimate)

  if (nonzero.coeffs.only) {
    glmnet.out.coeffs <- glmnet.out.coeffs %>%
      dplyr::filter(estimate != 0)
  }

  if (structural.filter.type[1] != "none") {
    coeff.mtx <- coef(glmnet.res)

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
  coeff.excl <- as.matrix(mtx == 0) ## TODO: be smarter

  # first coeff set - s0 - is always novel, we compare starting with s1
  coeff.excl.2 <- coeff.excl[, -1]

  # dimensions must match, we do remove the last column
  coeff.excl.3 <- coeff.excl[, -ncol(coeff.excl)]

  coeff.excl.same <- (coeff.excl.2 == coeff.excl.3)
  coeff.excl.same.reduced <- apply(coeff.excl.same, 2, all)
  # coeff.excl.same.reduced[i] == TRUE <-> coeffs[i+1] == coeffs[i]
  # coeff.excl.same.reduced[i] == FALSE <-> coeffs[i+1] != coeffs[i]
  #   that is, coeffs[i+1] are DIFFERENT   (***)

  coeff.excl.diff.prv <- which(!coeff.excl.same.reduced)
  # all steps with novelties

  # adding the s0, adding (+1) to account for the (***)
  coeff.excl.diff <- c(1, 1 + coeff.excl.diff.prv)
  subpaths.initial.elems <- coeff.excl.diff
  subpaths.final.elems <- c(coeff.excl.diff.prv, ncol(coeff.excl))

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

  coeff.mtx <- coef(model.full.lasso)
  changes.idx <- structural.changes.col.matrix(
    mtx = coeff.mtx,
    initial.substeps = TRUE,
    final.substeps = strict
    )

  lasso.struct.coeffs <- coeff.mtx[, changes.idx]
  Deviance <- deviance(model.full.lasso)[changes.idx]
  lasso.struct <- rbind(lasso.struct.coeffs, Deviance)
  lasso.struct
}

