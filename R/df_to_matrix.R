#' Non-factorial data.frame to matrix
#'
#' @param df.x (\code{data.frame}) to be converted
#' @param desc.basic the descriptor returned by \code{\link{df.describe.basic}}
df.as.matrix.dummy.nonfactor <- function(df.x, desc.basic){
  res.matrix <- as.matrix(df.x)
  desc.mat <- desc.basic %>%
    dplyr::mutate(
      col.level = NA,
      col.level.idx = NA,
      res.col.pos = 1:n(),
      res.col.name = colnames(res.matrix)
    ) %>%
    dplyr::select(
      org.col.idx = col.pos,
      res.col.idx = res.col.pos,
      org.col.name = col.name,
      res.col.name,
      factor.level = col.level,
      factor.level.idx = col.level.idx
    )

  ret <- list(
    mat.x = res.matrix,
    mat.x.transform = desc.mat
  )
  return(ret)
}

#' data.frame to matrix
#' @param df.x (\code{data.frame}) to be converted
#' @param desc.basic the descriptor returned by \code{\link{df.describe.basic}}
df.as.matrix.dummy.withfactor <- function(df.x, desc.basic){

  desc.basic <- desc.basic %>%
    dplyr::mutate(
      res.cols.used = ifelse(nlevels > 0, nlevels, 1),
      res.end.col = cumsum(res.cols.used),
      res.start.col = res.end.col - res.cols.used + 1
    )

  #number of factor variables
  nfact <- sum(desc.basic$nlevels > 0)

  #number of input cols
  nincols <- nrow(desc.basic)

  #number of non-factor variables
  nnonfact <- nincols - nfact

  #number of all levels of all factor variables
  nfactlevs <- sum(desc.basic$nlevels)

  #number of output columns
  noutcols <- nnonfact + nfactlevs

  sparse.cost <- 3*(nnonfact + nfact)
  dense.cost <- noutcols
  # would sparse representation be more memory-effective?
  use.sparse <- (sparse.cost < dense.cost)

  res.matrix <- Matrix::Matrix(0, nrow = nrow(df.x), ncol = noutcols, sparse = use.sparse)

  #for non-factors - simple as.matrix and redistribution
  all.non.fact.idx <- which(desc.basic$nlevels == 0)
  if (length(all.non.fact.idx) > 0) {
    all.non.fact.inmat <- desc.basic$col.pos[all.non.fact.idx]
    all.non.fact.outmat <- desc.basic$res.start.col[all.non.fact.idx]
    res.matrix[, all.non.fact.outmat] <- as.matrix(df.x[, all.non.fact.inmat])
  }

  #for factors - use Matrix::fac2sparse
  desc.mat.factors <- list()
  all.fact.idx <- which(desc.basic$nlevels > 0)
  for (desc.idx in all.fact.idx) {
    fac.desc <- desc.basic[desc.idx, ]
    inmat.col.idx <- fac.desc$col.pos
    outmat.col.idx <- (fac.desc$res.start.col):(fac.desc$res.end.col)
    fac.vals <- df.x[, inmat.col.idx]
    fac.vals.mat <- Matrix::fac2sparse(fac.vals, drop.unused.levels = FALSE)
    fac.sparse <- Matrix::t(fac.vals.mat)
    #read the levels
    fac.sparse.levels <- fac.sparse@Dimnames[[2]]
    desc.fact <- data.frame(
      col.name = fac.desc$col.name,
      col.pos = inmat.col.idx,
      col.level = fac.sparse.levels,
      col.level.idx = 1:length(fac.sparse.levels),
      res.col.pos = outmat.col.idx,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    desc.mat.factors <- c(desc.mat.factors, list(desc.fact))
    res.matrix[, outmat.col.idx] <- fac.sparse
  }

  desc.mat.factors.df <- dplyr::bind_rows(desc.mat.factors)

  if (nrow(desc.mat.factors.df) > 0) {
    desc.mat <- dplyr::left_join(
      desc.basic,
      desc.mat.factors.df,
      by = c("col.name", "col.pos")
    ) %>%
      dplyr::mutate(
        res.col.pos = ifelse(is.na(res.col.pos), res.start.col, res.col.pos),
        contrast.type = ifelse(is.na(col.level.idx), NA, "contrast.dummy"),
        res.col.name = ifelse(
          is.na(col.level.idx),
          col.name,
          paste0(col.name, col.level)
        )
      )
  } else {
    # no factors at all
    desc.mat <- desc.basic %>%
      dplyr::mutate(
        col.level = NA, col.level.idx = NA,
        res.col.pos = res.start.col, contrast.type = NA, res.col.name = col.name)
  }

  desc.mat <- desc.mat %>%
    dplyr::select(
      org.col.idx = col.pos,
      res.col.idx = res.col.pos,
      org.col.name = col.name,
      res.col.name,
      contrast.type,
      factor.level = col.level,
      factor.level.idx = col.level.idx
    ) %>%
    dplyr::arrange(res.col.idx)

  # renaming the cols in case of the column name mismatch
  desc.mat$res.col.name <- base::make.names(
    names = desc.mat$res.col.name,
    unique = TRUE,
    allow_ = TRUE
    )

  colnames(res.matrix) <- desc.mat$res.col.name

  list(
    mat.x = res.matrix,
    mat.x.transform = desc.mat
  )

}

#' Data frame to matrix
#' @export
df.as.matrix.dummy <- function(df.x){
  desc.basic <- df.describe.basic(df.x)

  #number of factor variables
  nfact <- sum(desc.basic$nlevels > 0)

  if (nrow(df.x) == 0 && ncol(df.x) == 0) {
    ret <- list(
      mat.x = Matrix::Matrix(0, nrow = 0, ncol = 0),
      mat.x.transform = data.frame(
        org.col.idx = integer(),
        res.col.idx = integer(),
        org.col.name = character(),
        res.col.name = character(),
        contrast.type = character(),
        factor.level = character(),
        factor.level.idx = integer()
        )
    )
    return(ret)
  }

  if (nfact == 0) {
    # no factor variables --> simple as.matrix will do the job
    ret <- df.as.matrix.dummy.nonfactor(
      df.x = df.x,
      desc.basic = desc.basic
    )
  } else {
    ret <- df.as.matrix.dummy.withfactor(
      df.x = df.x,
      desc.basic = desc.basic
    )
  }

  return(ret)
}

