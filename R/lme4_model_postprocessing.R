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
lme4_glmer_postprocess <- function(model.fit.lme4){

  # extract the conditional modes of the random effects from a fitted model object

  ranef.df <- lme4::ranef(model.fit.lme4, condVar = TRUE) %>% as.data.frame()
  fixef.df <- lme4::fixef(model.fit.lme4)  %>% as.data.frame()
  #variance-covariance matrix of the fixed effect terms
  vcov.df <- vcov(model.fit.lme4) %>% as.matrix() %>% as.data.frame()

  # estimated variances, standard deviations, and correlations between the random-effects terms in a mixed-effects model
  VarCorr.df <- lme4::VarCorr(model.fit.lme4) %>% as.data.frame()

  ranef.df.ext <- ranef.df %>%
    dplyr::mutate(
      term.num = as.numeric(term),
      term.chr = as.character(term),
      grp.num = as.numeric(grp),
      grp.chr = as.character(grp)
    )

  fixef.df.ext <- fixef.df
  fixef.df.ext$coef.name <- rownames(fixef.df)
  rownames(fixef.df.ext) <- NULL


  #frm <- do_model_framing_lme4(model = model.fit.lme4)

  list(
    ranef.df = ranef.df.ext,
    fixef.df = fixef.df.ext,
    VarCorr.df = VarCorr.df,
    vcov.df = vcov.df
  )


}




