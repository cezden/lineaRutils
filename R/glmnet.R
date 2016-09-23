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


#' @export
glmnet.path.inner <- function(df.xy, y.cols, ...){
  xy.col.names <- names(df.xy)
  x.col.idx <- !(xy.col.names %in% y.cols)
  mat.y <- df.xy %>%
    dplyr::select_(.dots = y.cols) %>%
    as.matrix()
  df.x <- df.xy[, x.col.idx]
  df.x.res <- df.as.matrix.dummy(df.x)
  mat.x <- df.x.res$mat.x

  glmnet.res <- glmnet::glmnet(x = mat.x, y = mat.y, ...)

  glmnet.out <- glmnet.process.output(glmnet.res)
  coeff.mtx <- coef(glmnet.res)
  #crit.steps.idx <- structural.changes.col.matrix(coeff.mtx, strict = FALSE)


}




#' Data frame to matrix conversion
#' @export
tmp.glmnet.path.inner <- function(df.xy, y.cols, ...){
  # mat.y <- df.xy %>%
  #   dplyr::select(time = surv.time, status = surv.event) %>%
  #   as.matrix()
  xy.col.names <- names(df.xy)
  x.col.idx <- (xy.col.names %nin% y.cols)
  mat.y <- df.xy %>%
    dplyr::select_(.dots = y.cols) %>%
    as.matrix()
  df.x <- df.xy[, x.col.idx]
  mat.x <- model.matrix(~. - 1, data = df.x)
  # no intercept, each level encoded
  mat.x.assign <- attr(mat.x, "assign")
  #mat.x <- mat.x[, mat.x.assign != 0]
  #mat.x.assign <- mat.x.assign[mat.x.assign != 0]
  res.translator <- data.frame(
    out.col = 1:length(mat.x.assign),
    in.col = mat.x.assign,
    in.col.name = names(df.x)[mat.x.assign],
    out.col.name = colnames(mat.x),
    stringsAsFactors = FALSE
  )

  tmp.fit <- glmnet::glmnet(mat.x, mat.y, ...)

  ttmp <- coef(tmp.fit) %>% as.matrix()
  ttmp.df <- t(ttmp) %>% as.data.frame()
  ttmp.df.n <- names(ttmp.df)
  col.trans <- data.frame(
    out.col.name = ttmp.df.n,
    out.col.name.idx = 1:length(ttmp.df.n),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::inner_join(res.translator, by = "out.col.name") %>%
    dplyr::arrange(out.col.name.idx)
  names(ttmp.df) <- col.trans$in.col.name
  ttmp.df$rowid <- 1:nrow(ttmp.df)
  ttmp.df.long <- tidyr::gather(
    data = ttmp.df,
    key = "variable",
    value = "value",
    -rowid
  ) %>%
    dplyr::filter(value != 0) %>%
    dplyr::mutate(value = 1)

  ttmp.df2 <- tidyr::spread(
    data = ttmp.df.long,
    key = "variable",
    value = "value",
    fill = 0
  )

  ttmp.df3 <- ttmp.df2 %>%
    dplyr::select(-rowid) %>%
    dplyr::distinct() #%>% dplyr::mutate(grpid = 1:n())
  ttmp.df3
}

tmp.glmnet.path <- function(model.data.xy){
  #model.data.xy <- models.data$pts.data.to.fit.os # !!
  model.data.xy.anyna <- any(is.na(model.data.xy))
  df.xy <- model.data.xy[complete.cases(model.data.xy), ]
  zz1 <- tmp.glmnet.path.inner(df.xy = df.xy, family = "cox")
  if (model.data.xy.anyna) {
    cc0 <- apply(model.data.xy, 2, function(x) all(!is.na(x)))
    df.xy2 <- model.data.xy[, cc0]
    zz2 <- tmp.glmnet.path.inner(df.xy = df.xy2)
    zz3 <- dplyr::bind_rows(zz1, zz2)
    zz3[is.na(zz3)] <- 0
    zz4 <- zz3 %>% dplyr::distinct()
  } else {
    zz4 <- zz1
  }
  zz4
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
#' @param model.full.lasso the model fitted by glmnet::glmnet function
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

