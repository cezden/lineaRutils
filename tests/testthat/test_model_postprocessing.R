context("Model postprocessing")

rstanarm_model_invariants <- function(m_fit_post, m_struct){
  testthat::expect_is(m_struct, "mixl_glmer_structure_spec")
  #testthat::expect_is(m_fit_post, "")

  testthat::expect_equivalent(
    m_fit_post$summary.df.left$coef.name, c("mean_PPD", "log-posterior")
    )

  randef.grps1 <- m_fit_post$ranef.df %>%
    dplyr::select(grpvar, term = term.chr) %>%
    dplyr::distinct() %>%
    dplyr::arrange(grpvar, term)
  randef.grps2 <- m_struct$mixmodel.formula.p$cnms.re %>%
    dplyr::select(grpvar, term) %>%
    dplyr::arrange(grpvar, term)

  testthat::expect_equivalent(randef.grps1, randef.grps2)

  fixeff.1 <- m_struct$mixmodel.formula.p$effects.fe
  fixeff.2 <- m_fit_post$fixef.df$coef.name

  testthat::expect_equivalent(fixeff.1, fixeff.2)


}


test_that("lme4",{

  testthat::expect_true(TRUE)

})


do_problem_framing <- function(model.formula, model.data, family){
  tttmp <- lme4_glmer_formulator(
    model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)",
    model.data = lme4::cbpp,
    model.params = list(family = family)
  )

}



test_that("rstanarm ~ 1 + (1 | herd) + (1 | herd:period)",{

  test.model.data <- lme4::cbpp
  #test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd)"
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)"
  test.model.params <- list(
    #family = "binomial",
    chains = 2,
    iter = 100,
    thin = 1,
    open_progress = FALSE,
    show_messages = FALSE
  )

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = list(family = "binomial")
  )

  testthat::expect_is(test.model.struct, "mixl_glmer_structure_spec")


  testthat::capture_output({
    model.fit2 <- rstanarm_fit(
      test.model.struct,
      test.model.params
    )
  })

  model.fit2.post <- rstanarm_glmer_postprocess(model.fit2)

  rstanarm_model_invariants(
    m_fit_post = model.fit2.post,
    m_struct = test.model.struct
    )

  return()



  class(model.fit2)
  rstanarm::ngrps(model.fit2)
  coef(model.fit2)
  rstanarm::ranef(model.fit2)  %>% as.data.frame()
  rstanarm::fixef(model.fit2)  %>% as.data.frame()
  VarCorr(model.fit2) %>% as.data.frame()
  vcov(model.fit2) %>% as.data.frame()
  plot(model.fit2)






  sleepstudy.mod <- lme4::sleepstudy %>%
    dplyr::mutate(
      Reaction.dicho = Reaction<median(Reaction)
    )

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = "Reaction.dicho ~ Days + (Days | Subject)",
    model.data = sleepstudy.mod,
    model.params = list(family = "binomial")
  )

  #lme4_glmer_formulator_parser(tttmp)

  test.model.params.stan <- list(
    chains = 2,
    iter = 100,
    thin = 1,
    open_progress = FALSE,
    show_messages = FALSE
  )

  testthat::capture_output({
    model.fit2 <- rstanarm_fit(
      test.model.struct,
      test.model.params.stan
    )
  })

  model.fit2.post <- rstanarm_glmer_postprocess(model.fit2)

  rstanarm_model_invariants(
    m_fit_post = model.fit2.post,
    m_struct = test.model.struct
  )

  model.fit2.post$vcov.df

  fit <- glmer(Reaction.dicho ~ Days + (Days | Subject),
               lme4::sleepstudy,
               family = binomial("logit"))


  model.fit.stan <- rstanarm_glmer_fit(
    model.formula = "Reaction.dicho ~ Days + (Days | Subject)",
    model.data = sleepstudy.mod,
    model.params = test.model.params.stan
  )



  model.fit2.post <- rstanarm_glmer_postprocess(model.fit2)
  model.fit2.post$fixef.df
  model.fit2.post$summary.df.left
  model.fit2.post$VarCorr.df


  tttmp <- mixl_glmer_structure_spec(
    model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)",
    model.data = lme4::cbpp,
    model.params = list(family = "binomial")
  )

  tttmp <- mixl_glmer_structure_spec(
    model.formula = "Reaction.dicho ~ Days + (Days | Subject)",
    model.data = sleepstudy.mod,
    model.params = list(family = "binomial")
  )

  broom::tidy(tttmp)


  tttmpz <- do_model_framing_lme4(tttmp)





  m <- lme4::glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
               (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"))


  lineaRutils::hdp


  tttmp <- mixl_glmer_structure_spec(
    model.formula = "remission ~ (1 | CancerStage:Sex) + (1 | DID)",
    model.data = ds_hdp,
    model.params = list(family = "binomial")
  )

  tttmp$mixmodel.formula$reTrms$cnms

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = "remission ~ (1 | CancerStage) + (1 | Sex) + (1 | CancerStage:Sex) + (1 | DID)",
    model.data = ds_hdp,
    model.params = list(family = "binomial")
  )

  testthat::capture_output({
    model.fit2 <- rstanarm_fit(
      test.model.struct,
      test.model.params
    )
  })

  model.fit2.post <- rstanarm_glmer_postprocess(model.fit2)
  model.fit2.post$summary.df.left

  model.fit2.post$fixef.df
  tmp <- model.fit2.post$ranef.df


  tttmp <- mixl_glmer_structure_spec(
    model.formula = "remission ~ (1 + Married | Sex) + (1 | CancerStage:Sex) + (1 | DID)",
    model.data = ds_hdp,
    model.params = list(family = "binomial")
  )

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = "remission ~ CRP + (1 + CRP | Sex) + (1 | CancerStage:Sex) + (1 | DID)",
    model.data = ds_hdp,
    model.params = list(family = "binomial")
  )

  tttmp <- mixl_glmer_structure_spec(
    model.formula = "remission ~ Married + (1 + CRP | Sex) + (1 | CancerStage:Sex)",
    model.data = ds_hdp,
    model.params = list(family = "binomial")
  )


  tttmp$mixmodel.formula.p$cnms.re
  tmp <- tttmp$mixmodel.formula.p$Ztlist.re
  tttmp$mixmodel.formula.p$effects.fe
  names(tttmp$mixmodel.formula$reTrms$flist)

  ## list of factors (can read values!)
  tmp <- tttmp$mixmodel.formula$reTrms$flist
  ##
  tmp <- tttmp$mixmodel.formula$reTrms$Zt
  tmp <- tttmp$mixmodel.formula$reTrms$Ztlist
  tmp <- tttmp$mixmodel.formula$reTrms
  tmp <- tttmp$mixmodel.formula$X
  names(tmp)
  colnames(tmp)
  rownames(tmp$`1 | CancerStage:Sex`)

  rstanarm_pad_reTrms(
    tttmp$mixmodel.formula$reTrms$Ztlist,
    tttmp$mixmodel.formula$reTrms$cnms,
    tttmp$mixmodel.formula$reTrms$flist)




  tmp <- tttmp$mixmodel.formula$reTrms$theta
  tmp <- tttmp$mixmodel.formula$reTrms$Lind
  tmp <- tttmp$mixmodel.formula$reTrms$flist




    summary(tttmp)



  tttmpz <- do_model_framing_lme4(tttmp)
  redf <- tttmpz$RE.df

  tttmp@cnms


  names(tttmpz$RE.df)

  summary(hdp)


  summary(tttmp)



  tttmp.ml <- list()
  tttmp.ml[[1]] <- tttmp
  class(tttmp.ml) <- "merModList"

  merTools::modelRandEffStats(tttmp.ml)
  merTools::modelInfo(tttmp.ml)

  glmerModList()

  tttmp$reTrms$theta


  #https://rdrr.io/cran/lme4/man/mkReTrms.html

  tttmp$reTrms$flist



  tttmp$reTrms$Ztlist



  #install.packages(c("Rcpp", "Matrix", "RcppRoll", "RcppEigen", "RcppArmadillo", "RcppParallel"), Ncpus = 3)
  #install.packages(c("RcppParallel"))

  #install.packages(c("tidyverse"), Ncpus = 3)
  #install.packages(c("devtools"), Ncpus = 3)
  #install.packages(c("data.table", "feather"), Ncpus = 3)
  #install.packages(c("BH", "bindrcpp", "", "", ""))
  #install.packages(c("VGAM", "rstan", "lme4"), Ncpus = 3)
  #install.packages(c("glmnet", "glmpath"), Ncpus = 3)
  #install.packages(c("brms", "rstanarm", "arm", "blme", "coda", "coin", "conting", "broom","bridgesampling","BMS"), Ncpus = 3)
  #install.packages(c("BayesGOF", "betareg", "diagis","flexmix","gdata", "Hmisc", "prophet"), Ncpus = 3)




  tttmp <- lme4::glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = cbpp, family = binomial)
  names(tttmp)

  tttmp2 <- lme4::glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd) + (1 | herd:period),
                           data = cbpp, family = binomial)
  names(tttmp2)

  head(tttmp2$fr)
  names(tttmp2$reTrms)
  tttmp2$reTrms$cnms

  names(model.fit2)

  tttmp2$reTrms$flist

  head(tttmp2$reTrms$flist)
  attr(tttmp2$reTrms$flist, "assign")

  tttmp2$reTrms$Zt

  pad_reTrms_l <- sapply(attr(tttmp2$reTrms$flist, "assign"), function(i) nlevels(tttmp2$reTrms$flist[[i]]))

  library(Matrix)
  rstanarm_pad_reTrms(
    Ztlist = tttmp2$reTrms$Ztlist,
    cnms = tttmp2$reTrms$cnms,
    flist = tttmp2$reTrms$flist
    )


  tttmp2$reTrms$Lind

  #rstanarm::stan_glmer()

  model.fit2.sum <- summary(model.fit2)
  rownames(model.fit2.sum)

  broom::glance(model.fit2)
  param.types <- c("non-varying", "varying", "hierarchical", "auxiliary")
  broom::tidy(model.fit2, intervals = TRUE, parameters = param.types[2])
  broom::tidy(model.fit2, intervals = TRUE, parameters = param.types[3])

  # https://github.com/mjskay/tidybayes/blob/master/vignettes/tidy-rstanarm.Rmd

  str(model.fit2.sum)


  model.fit2.sum.df <- model.fit2.sum %>% as.data.frame()

  zz1 <- fitted(model.fit2)

  rstanarm::ranef(model.fit2)
  rstanarm::fixef(model.fit2)

  #coef(model.fit2)
  rstanarm::posterior_interval(model.fit2)
  rstanarm::se(model.fit2)
  #confint(model.fit2)
  sigma(model.fit2)

  #str(rstanarm::prior_summary(model.fit2))





})

test_that("rstanarm, Reaction.dicho ~ Days + (Days | Subject)",{

  test.model.formula <- "Reaction.dicho ~ Days + (Days | Subject)"
  test.model.data <- lme4::sleepstudy %>%
    dplyr::mutate(
      Reaction.dicho = Reaction<median(Reaction)
    )

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = list(family = "binomial")
  )

  test.model.params <- list(
    #family = "binomial",
    chains = 2,
    iter = 100,
    thin = 1,
    open_progress = FALSE,
    show_messages = FALSE
  )

  testthat::capture_output({
    model.fit2 <- rstanarm_fit(
      test.model.struct,
      test.model.params
    )
  })

  model.fit2.post <- rstanarm_glmer_postprocess(model.fit2)

  rstanarm_model_invariants(
    m_fit_post = model.fit2.post,
    m_struct = test.model.struct
  )

})

test_that("rstanarm, hdp",{

  return()

  test.model.formula <- "remission ~ (1 | CancerStage:Sex) + (1 | DID)"
  test.model.data <- lineaRutils::ds_hdp

  test.model.struct <- mixl_glmer_structure_spec(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = list(family = "binomial")
  )

  test.model.params <- list(
    chains = 2,
    iter = 100,
    thin = 1,
    open_progress = FALSE,
    show_messages = FALSE
  )

  testthat::capture_output({
    model.fit.stan <- rstanarm_fit(
      test.model.struct,
      test.model.params
    )
  })

  model.fit.lme4 <- lme4_fit(
    test.model.struct,
    test.model.params = list()
  )

  model.fit.stan.post <- rstanarm_glmer_postprocess(model.fit.stan)
  do_model_framing_lme4()

  rstanarm_model_invariants(
    m_fit_post = model.fit.stan.post,
    m_struct = test.model.struct
  )

})



