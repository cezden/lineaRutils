tmptmptmptmp <- function(){
  # factor2dict
  zz <- rep(LETTERS[1:10], 3)
  zz.factor <- as.factor(zz)
  f.subset <- unique(zz.factor)
  data.frame(code = as.integer(f.subset), label = as.character(f.subset), stringsAsFactors = FALSE)

  as.integer(f.subset)
  as.character(f.subset)
  levels(zz.factor)
  as.numeric(zz.factor[1])
  str(zz.factor)
  df.describe.basic(airquality)
  aqq <- airquality
  #aqq$Month <- factor(airquality$Month, ordered = TRUE)
  aqq$Month <- factor(airquality$Month)
  aqq$Day <- factor(airquality$Day)
  aqq$vvv <- rep(letters, 20)[nrow(aqq)]
  aqq$vvv2 <- (aqq$vvv == "a")
  tmp <- df.describe.basic(aqq)
  tmp2 <- df.describe.factors(aqq)
  tmp3 <- df.describe.factors(airquality)
  str(Hmisc::contents(aqq))
  tmp1 <- glmnet.df.as.matrix(aqq, use.and.remove.intercept = FALSE)
  tmp11 <- tmp1$mat.x.transform
  tmp12 <- tmp1$mat.x
  tmp2 <- glmnet.df.as.matrix(aqq, use.and.remove.intercept = TRUE)
  tmp21 <- tmp2$mat.x.transform
  tmp22 <- tmp2$mat.x
  tmp3 <- df.as.matrix.dummy(aqq)
  tmp31 <- tmp3$mat.x.transform
  tmp32 <- tmp3$mat.x %>% as.matrix()

  aqq2 <- airquality %>% dplyr::select(Ozone, Month, Solar.R, Day, Wind, Temp)
  aqq2$Month <- factor(airquality$Month)
  aqq2$Day <- factor(airquality$Day)
  tmp4 <- df.as.matrix.dummy(aqq2)
  tmp4tmp <- tmp4$mat.x.transform


  names(airquality)
  tmp.tmp <- model.matrix(~. + Month:Day, data = df.x)
  attr(tmp.tmp, "assign")
  str(tmp.tmp)
}





#' Data frame to matrix conversion
#'
#' Simple conversion of \code{data.frame}, with default factors encoding,
#' using the \code{\link[stats]{model.matrix}} with formula \code{(~.)} or \code{(~. - 1)},
#' and removal of resulting \code{intercept}
#' column (if any).
#'
#' If the formula \code{(~.)} is used (\code{use.and.remove.intercept = TRUE}):
#' \itemize{
#'  \item{each factor is contrasted \emph{vs} its first level, that is}
#'  \item{the first level of each factor is dropped and all the remaining levels are encoded
#'  as the binary columns in the resulting matrix}
#' }
#'
#' Function provides the description of the transformation, mapping the columns and factor levels to
#' the resulting matrix.
#'
#' @param df.x (\code{data.frame}) to be encoded
#' @param use.and.remove.intercept if \code{TRUE} the
#' @export
glmnet.df.as.matrix <- function(df.x, use.and.remove.intercept = FALSE){

  if (use.and.remove.intercept) {
    mat.x <- model.matrix(~., data = df.x)
    mat.x.assign <- attr(mat.x, "assign")
    mat.x.contrasts <- attr(mat.x, "contrasts")
    # removal of the intercept
    mat.x <- mat.x[, mat.x.assign != 0]
    mat.x.assign <- mat.x.assign[mat.x.assign != 0]
  } else {
    mat.x <- model.matrix(~. - 1, data = df.x)
    # no intercept, each level encoded
    mat.x.assign <- attr(mat.x, "assign")
    mat.x.contrasts <- attr(mat.x, "contrasts")
  }
  res.translator <- data.frame(
    org.col.idx = mat.x.assign,
    res.col.idx = 1:length(mat.x.assign),
    org.col.name = names(df.x)[mat.x.assign],
    res.col.name = colnames(mat.x),
    stringsAsFactors = FALSE
  )
  if (!is.null(mat.x.contrasts)) {
    con.mat.df <- data.frame(
      org.col.name = names(mat.x.contrasts),
      contrast.type = unlist(mat.x.contrasts),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    res.translator <- res.translator %>%
      dplyr::left_join(
        con.mat.df,
        by = "org.col.name"
      )
  }
  res.translator <- res.translator %>%
    dplyr::mutate(
      factor.level = stringi::stri_replace_first_fixed(
        res.col.name,
        org.col.name,
        ""
        )
    )

  list(
    df.x = df.x,
    mat.x = mat.x,
    mat.x.transform = res.translator
  )
}

#' The glmnet postprocessor
#'
#' The function is based on the results returned by \code{\link[broom]{glmnet_tidiers}}
#'
#' @param glmnet.res the result of the \code{\link[glmnet]{glmnet}} function
#' @return the \code{list} with following fields:
#'  \itemize{
#'    \item{\code{steps}: a \code{data.frame} with columns:
#'      \itemize{
#'        \item{\code{step}}
#'        \item{\code{lambda}}
#'        \item{\code{dev.ratio}}
#'        \item{\code{df}}
#'        \item{\code{deviance}}
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
glmnet.process.output <- function(glmnet.res){
  glmnet.out <- broom::tidy(glmnet.res)
  glmnet.out.steps <- glmnet.out %>%
    dplyr::select(step, lambda, dev.ratio) %>%
    dplyr::distinct() %>%
    dplyr::arrange(step) %>%
    dplyr::mutate(
      df = glmnet.res$df,
      deviance = deviance(glmnet.res)
    )

  glmnet.out.coeffs <- glmnet.out %>%
    dplyr::select(step, term, estimate)

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
  crit.steps.idx <- structural.changes.col.matrix(coeff.mtx, strict = FALSE)


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
structural.changes.col.matrix <- function(mtx, strict = FALSE) {
  if (ncol(mtx) == 1) {
    return(c(1))
  }
  # getting the inclusion/exclusion matrix
  coeff.excl <- as.matrix(mtx == 0)
  # first coeff set - s0 - is always novel, we compare starting with s1
  coeff.excl.2 <- coeff.excl[, -1]
  # dimensions must match, we do remove the last column
  coeff.excl.3 <- coeff.excl[, -ncol(coeff.excl)]

  coeff.excl.same <- (coeff.excl.2 == coeff.excl.3)
  coeff.excl.same.reduced <- apply(coeff.excl.same, 2, all)
  # coeff.excl.same.reduced[i] == TRUE <-> coeffs[i+1] == coeffs[i]
  # coeff.excl.same.reduced[i] == FALSE <-> coeffs[i+1] != coeffs[i]
  #   that is, coeffs[i+1] are DIFFERENT   (*)
  coeff.excl.diff.prv <- which(!coeff.excl.same.reduced)
  # all steps with novelties

  # adding the s0, adding (+1) to account for the (*)
  coeff.excl.diff <- c(1, 1 + coeff.excl.diff.prv)
  if (!strict) {
    #let's also add one before the structural change
    #let's also include last one if not prev. included
    coeff.excl.diff <- c(
      coeff.excl.diff,
      coeff.excl.diff.prv,
      ncol(coeff.excl)
    )
  }
  # removing duplicates, which might occur in (!strict) case
  coeff.excl.diff.out <- sort(unique(coeff.excl.diff))
  coeff.excl.diff.out
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
    strict = strict
    )

  lasso.struct.coeffs <- coeff.mtx[, changes.idx]
  Deviance <- deviance(model.full.lasso)[changes.idx]
  lasso.struct <- rbind(lasso.struct.coeffs, Deviance)
  lasso.struct
}

