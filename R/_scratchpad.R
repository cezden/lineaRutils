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

  library(rvest)

  base.name <- "http://www.sejm.gov.pl/sejm7.nsf/agent.xsp?symbol=klubdecglos&IdGlosowania=39850&KodKlubu=PO&Decyzja="
  decision.var <- "Decyzja"
  decisions <- c("Za", "Przeciw", "Wstrzym", "Nieobecny")
  party.club <- "KodKlubu"

  party.clubc <- c("PO", "PiS", "RP", "PSL", "SLD", "SP", "niez.", "ID")


  base.name <- "http://www.sejm.gov.pl/sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=39850&KodKlubu=SP"
  tdist <- xml2::read_html(base.name)

  tdist %>%
    html_node("table.view:_id1:_id2:facetMain:agentHTML") %>%
    html_table(header = FALSE)

  tdist %>%
    rvest::html_node("#view:_id1:_id2:facetMain:agentHTML > table")

  tdist %>%
    rvest::html_node("#view:_id1:_id2:facetMain:agentHTML > table")

  res <- tdist %>%
    rvest::html_node(xpath = '//*[@id="view:_id1:_id2:facetMain:agentHTML"]/table') %>%
    html_table(header = FALSE, fill = TRUE)

  res2 <- res[-1, ]

  res3 <- dplyr::bind_rows(
    voret.name =
  )

  #


  rvest::html_table()

  vvect <- runif(1000*10)
  vvect[vvect<0.6] <- 0

  vmat <- Matrix::Matrix(vvect, nrow = 1000)
  vmat0 <- vmat[, -1]
  str(vmat0)
  vmat.l <- vmat[, -ncol(vmat)]
  str(vmat.l)
  vmat0.0 <- abs((sign(vmat0)))
  vmatl.l <- abs((sign(vmat.l)))
  str(vmat0.0)
  vvmat <- (vmat0.0 - vmatl.l)
  vvmat <- (vmat0 == vmat.l)
  str(vvmat)
  str(vmat)


  trans1 <- dplyr::bind_rows(
    data.frame(
      p.setup.w.cm = 70.01, p.setup.h.cm = 100.01,
      export.w.px = 720, export.h.px = 1028,
      out.w.px = 720, out.h.px = 1028, out.dpi = 72),
    data.frame(
      p.setup.w.cm = 70.01, p.setup.h.cm = 100.01,
      export.w.px = 1440, export.h.px = 2057,
      out.w.px = 1440, out.h.px = 2057, out.dpi = 72),
    data.frame(
      p.setup.w.cm = 70.01, p.setup.h.cm = 100.01,
      export.w.px = 1985, export.h.px = 2835,
      out.w.px = 1985, out.h.px = 2835, out.dpi = 72),
    data.frame(
      p.setup.w.cm = 70.01, p.setup.h.cm = 100.01,
      export.w.px = 1575, export.h.px = 2249,
      out.w.px = 1985, out.h.px = 2835, out.dpi = 72)

  )

  trans1.size <- trans1 %>%
    dplyr::mutate(
      p.setup.w.in = p.setup.w.cm/2.54,
      p.setup.h.in = p.setup.h.cm/2.54,
      p.setup.w.px = p.setup.w.in*72,
      p.setup.h.px = p.setup.h.in*72,
      aspect = export.h.px/out.w.px,
      out.w.in = out.w.px / out.dpi,
      out.h.in = out.h.px / out.dpi,
      out.w.cm = out.w.in * 2.54,
      out.h.cm = out.h.in * 2.54,
      diagonal.len.in = sqrt(out.w.in^2 + out.h.in^2)
    )
  trans1.size


  #procedure:
  # 1. export to PDF
  # 2. from PDF using Preview choose the format, ppi = 200, HQ
  #

  trans2 <- dplyr::bind_rows(
    data.frame(
      p.setup.w.cm = 70.01, p.setup.h.cm = 100.01,
      pdf.w.cm = 70.03, pdf.h.cm = 100.02,
      export.ppi = 200,
      out.w.px = 5513, out.h.px = 7875, out.dpi = 200)
  )

  trans2.size <- trans2 %>%
    dplyr::mutate(
      p.setup.w.in = p.setup.w.cm/2.54,
      p.setup.h.in = p.setup.h.cm/2.54,
      pdf.w.px = pdf.w.cm/2.54*export.ppi,
      pdf.h.px = pdf.h.cm/2.54*export.ppi,
      aspect = pdf.h.px/pdf.w.px,
      out.w.in = out.w.px / out.dpi,
      out.h.in = out.h.px / out.dpi,
      out.w.cm = out.w.in * 2.54,
      out.h.cm = out.h.in * 2.54,
      diagonal.len.in = sqrt(out.w.in^2 + out.h.in^2)
    )
  trans2.size

  fullHD <- c(1080, 1920)
  c(1080, 1920)/96 * 2.54
  fullHD.prop <- fullHD / numbers::mGCD(fullHD)

  defaultD <- c(720, 1028)
  defaultD.prop <- defaultD / numbers::mGCD(defaultD) # px
  180/257

  in.cm <- c(100, 254)
  in.cm.red <- in.cm / (100*numbers::mGCD(in.cm))

  fullHD.prop * in.cm.red[2] *5
  fullHD.prop * in.cm.red[1] *5 * 2.54

  254 * fullHD.prop

  ttmp <- binomial()
  str(ttmp)



}


# ' Data frame to matrix conversion
# '
# ' Simple conversion of \code{data.frame}, with default factors encoding,
# ' using the \code{\link[stats]{model.matrix}} with formula \code{(~.)} or \code{(~. - 1)},
# ' and removal of resulting \code{intercept}
# ' column (if any).
# '
# ' If the formula \code{(~.)} is used (\code{use.and.remove.intercept = TRUE}):
# ' \itemize{
# '  \item{each factor is contrasted \emph{vs} its first level, that is}
# '  \item{the first level of each factor is dropped and all the remaining levels are encoded
# '  as the binary columns in the resulting matrix}
# ' }
# '
# ' Function provides the description of the transformation, mapping the columns and factor levels to
# ' the resulting matrix.
# '
# ' @ param df.x (\code{data.frame}) to be encoded
# ' @ param use.and.remove.intercept if \code{TRUE} the
# ' @ export
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




# ' Data frame to matrix conversion
# ' @export
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


