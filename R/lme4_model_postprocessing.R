#' @export
lme4_glmer_formulator_parser <- function(mix_glForm){
  cnms <- mix_glForm$reTrms$cnms
  # a list of column names of the random effects according to the grouping factors
  cnms.re <- lapply(
    names(cnms),
    function(mix.comp.name){
      data.frame(
        term = cnms[[mix.comp.name]],
        grpvar = mix.comp.name,
        stringsAsFactors = FALSE
      )
    }
  ) %>% dplyr::bind_rows()

  Ztlist <- mix_glForm$reTrms$Ztlist
  # a list of column names of the random effects according to the grouping factors
  Ztlist.re <- lapply(
    names(Ztlist),
    function(mix.comp.name){
      data.frame(
        factor_value = rownames(Ztlist[[mix.comp.name]]),
        component = mix.comp.name,
        stringsAsFactors = FALSE
      )
    }
  ) %>% dplyr::bind_rows()

  list(
    cnms.re = cnms.re,
    Ztlist.re = Ztlist.re,
    effects.fe = colnames(mix_glForm$X)
  )
}


#' @export
mixl_glmer_structure_spec <- function(model.formula, model.data, model.params){
  # binding environment
  model.form <- as.formula(model.formula)
  model.params$data <- model.data
  model.params$formula <- model.form
  mixmodel_formula <- do.call(what = lme4::glFormula, args = model.params)
  ret <- list(
    model.params = model.params,
    mixmodel.formula = mixmodel_formula,
    mixmodel.formula.p = lme4_glmer_formulator_parser(mixmodel_formula)
  )
  class(ret) <- 'mixl_glmer_structure_spec'

  ret
}





#' @export
do_model_framing_lme4 <- function(model){

  mod_RE = merTools::REextract(model)
  mod_RE_df = mod_RE %>%
    tidyr::gather(
      key = "variable",
      val = "value",
      -groupFctr, -groupID
    )

  list(
    RE = mod_RE,
    REsd = merTools::REsdExtract(model),
    RE.df = mod_RE_df
  )
}




#' @export
rstanarm_glmer <- function (formula, data = NULL, family = gaussian, subset, weights,
                            na.action = getOption("na.action", "na.omit"), offset, contrasts = NULL,
                            ..., prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
                            prior_covariance = decov(), prior_PD = FALSE, algorithm = c("sampling",
                                                                                        "meanfield", "fullrank"), adapt_delta = NULL, QR = FALSE,
                            sparse = FALSE)
{
  call <- match.call(expand.dots = TRUE)
  mc <- match.call(expand.dots = FALSE)
  data <- validate_data(data)
  family <- validate_family(family)
  mc[[1]] <- quote(lme4::glFormula)
  mc$control <- make_glmerControl()
  mc$prior <- mc$prior_intercept <- mc$prior_covariance <- mc$prior_aux <- mc$prior_PD <- mc$algorithm <- mc$scale <- mc$concentration <- mc$shape <- mc$adapt_delta <- mc$... <- mc$QR <- mc$sparse <- NULL
  glmod <- eval(mc, parent.frame())
  X <- glmod$X
  y <- glmod$fr[, as.character(glmod$formula[2L])]
  if (is.matrix(y) && ncol(y) == 1L)
    y <- as.vector(y)
  offset <- model.offset(glmod$fr) %ORifNULL% double(0)
  weights <- validate_weights(weights)
  if (is.null(prior))
    prior <- list()
  if (is.null(prior_intercept))
    prior_intercept <- list()
  if (is.null(prior_aux))
    prior_aux <- list()
  if (is.null(prior_covariance))
    stop("'prior_covariance' can't be NULL.", call. = FALSE)
  group <- glmod$reTrms
  group$decov <- prior_covariance
  algorithm <- match.arg(algorithm)
  stanfit <- rstanarm::stan_glm.fit(x = X, y = y, weights = weights,
                                    offset = offset, family = family, prior = prior, prior_intercept = prior_intercept,
                                    prior_aux = prior_aux, prior_PD = prior_PD, algorithm = algorithm,
                                    adapt_delta = adapt_delta, group = group, QR = QR, sparse = sparse,
                                    ...)
  if (family$family == "Beta regression")
    family$family <- "beta"
  sel <- apply(X, 2L, function(x) !all(x == 1) && length(unique(x)) <
                 2)
  X <- X[, !sel, drop = FALSE]
  Z <- pad_reTrms(Ztlist = group$Ztlist, cnms = group$cnms,
                  flist = group$flist)$Z
  colnames(Z) <- b_names(names(stanfit), value = TRUE)
  fit <- nlist(stanfit, family, formula, offset, weights,
               x = if (getRversion() < "3.2.0")
                 cBind(X, Z)
               else cbind2(X, Z), y = y, data, call, terms = NULL,
               model = NULL, na.action = attr(glmod$fr, "na.action"),
               contrasts, algorithm, glmod, stan_function = "stan_glmer")
  out <- stanreg(fit)
  class(out) <- c(class(out), "lmerMod")
  return(out)
}

#' @export
rstanarm_pad_reTrms <- function (Ztlist, cnms, flist)
{
  stopifnot(is.list(Ztlist))
  l <- sapply(attr(flist, "assign"), function(i) nlevels(flist[[i]]))
  p <- sapply(cnms, FUN = length)
  n <- ncol(Ztlist[[1]])
  for (i in attr(flist, "assign")) {
    levels(flist[[i]]) <- c(gsub(" ", "_", levels(flist[[i]])),
                            paste0("_NEW_", names(flist)[i]))
  }
  for (i in 1:length(p)) {
    Ztlist[[i]] <- if (getRversion() < "3.2.0") {
      rBind(Ztlist[[i]], Matrix(0, nrow = p[i], ncol = n,
                                sparse = TRUE))
    }
    else {
      rbind2(Ztlist[[i]], Matrix::Matrix(0, nrow = p[i], ncol = n,
                                         sparse = TRUE))
    }
  }
  Z <- Matrix::t(do.call(rbind, args = Ztlist))
  return(rstanarm_nlist(Z, cnms, flist))
}

#' @export
rstanarm_nlist <- function (...)
{
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names)
    FALSE
  else nzchar(names(out))
  if (all(has_name))
    return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  }
  else {
    names(out)[!has_name] <- nms[!has_name]
  }
  return(out)
}
