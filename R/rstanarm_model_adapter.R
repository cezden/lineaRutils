#' @export
rstanarm_glmer_fit <- function(model.formula, model.data, model.params){
  # binding environment
  model.form <- as.formula(model.formula)
  model.params$data <- model.data
  model.params$formula <- model.form
  mixmodel <- do.call(what = rstanarm::stan_glmer, args = model.params)
  mixmodel
}

#' @export
rstanarm_fit.mixl_glmer_structure_spec <- function(glmer_spec, fit.params){
  model.params <- c(glmer_spec$model.params, fit.params)
  mixmodel <- do.call(what = rstanarm::stan_glmer, args = model.params)
  mixmodel
}

#' @export
rstanarm_glmer_postprocess <- function(model.fit2){

  #class(model.fit2)
  rstanarm::ngrps(model.fit2)
  #coef(model.fit2)
  ranef.df <- rstanarm::ranef(model.fit2)  %>% as.data.frame()
  fixef.df <- rstanarm::fixef(model.fit2)  %>% as.data.frame()
  vcov.df <- vcov(model.fit2) %>% as.data.frame()
  VarCorr.df <- VarCorr(model.fit2) %>% as.data.frame()
  #head(ranef.df)

  ranef.df.ext <- ranef.df %>%
    dplyr::mutate(
      term.num = as.numeric(term),
      term.chr = as.character(term),
      grp.num = as.numeric(grp),
      grp.chr = as.character(grp)
    ) %>%
    dplyr::mutate(
      grp.ind = paste0(grpvar, ":", grp),
      coef.name = paste0("b[", term, " ", grp.ind, "]")
    ) %>%
    dplyr::select(-condval)

  fixef.df.ext <- fixef.df
  fixef.df.ext$coef.name <- rownames(fixef.df)
  rownames(fixef.df.ext) <- NULL

  VarCorr.df.ext.1 <- VarCorr.df %>%
    dplyr::select(grp, var1, var2) %>%
    dplyr::mutate(
      var1.eff = var1,
      var2.eff = ifelse(is.na(var2), var1.eff, var2)
    )
  VarCorr.df.ext.1.revord <- VarCorr.df.ext.1 %>%
    dplyr::rename(
      var1.eff = var2.eff,
      var2.eff = var1.eff
    )
  VarCorr.df.ext <- dplyr::bind_rows(
    VarCorr.df.ext.1,
    VarCorr.df.ext.1.revord
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      val.name = paste0(var1.eff, ",", var2.eff),
      coef.name = paste0("Sigma[", grp, ":", val.name, "]")
    )


  summary.df <- summary(model.fit2) %>% as.data.frame()
  summary.df$coef.name <- rownames(summary.df)
  rownames(summary.df) <- NULL

  ranef.df.out <- dplyr::inner_join(
    ranef.df.ext,
    summary.df,
    by = "coef.name"
  )

  fixef.df.out <- dplyr::inner_join(
    fixef.df.ext,
    summary.df,
    by = "coef.name"
  )

  Sigma.df.out <- dplyr::inner_join(
    VarCorr.df.ext,
    summary.df,
    by = "coef.name"
  )


  #https://tjmahr.github.io/visualizing-uncertainty-rstanarm/

  #summary(model.fit2, pars = c("alpha", "beta"))
  #summary(model.fit2, pars = c("varying"))

  summary.df.left <- summary.df %>%
    dplyr::filter(!(coef.name %in% ranef.df.ext$coef.name)) %>%
    dplyr::filter(!(coef.name %in% fixef.df.ext$coef.name)) %>%
    dplyr::filter(!(coef.name %in% VarCorr.df.ext$coef.name))

  list(
    ranef.df = ranef.df.out,
    fixef.df = fixef.df.out,
    Sigma.df = Sigma.df.out,
    summary.df.left = summary.df.left,
    VarCorr.df = VarCorr.df,
    vcov.df = vcov.df
  )


}

