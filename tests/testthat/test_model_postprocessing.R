context("Model postprocessing")

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



test_that("rstanarm 1",{

  test.model.data <- lme4::cbpp
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd)"
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)"
  test.model.params <- list(
    family = "binomial",
    chains = 4,
    iter = 1000,
    thin = 2,
    open_progress = FALSE,
    show_messages = FALSE
  )

  model.fit2 <- rstanarm_glmer_fit(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = test.model.params
  )

  do_model_framing(
    model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)",
    model.data = lme4::cbpp,
    family = "binomial"
  )

  tttmp <- lme4_glmer_formulator(
    model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)",
    model.data = lme4::cbpp,
    model.params = list(family = "binomial")
  )

  fit <- glmer(Reaction.dicho ~ Days + (Days | Subject),
               lme4::sleepstudy,
               family = binomial("logit"))

  sleepstudy.mod <- lme4::sleepstudy %>%
    dplyr::mutate(
      Reaction.dicho = Reaction<median(Reaction)
    )

  tttmp <- lme4_glmer_formulator(
    model.formula = "Reaction.dicho ~ Days + (Days | Subject)",
    model.data = sleepstudy.mod,
    model.params = list(family = "binomial")
  )

  lme4_glmer_formulator_parser(tttmp)

  tttmp <- lme4_glmer_fit(
    model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd) + (1 | herd:period)",
    model.data = lme4::cbpp,
    model.params = list(family = "binomial")
  )

  tttmp <- lme4_glmer_fit(
    model.formula = "Reaction.dicho ~ Days + (Days | Subject)",
    model.data = sleepstudy.mod,
    model.params = list(family = "binomial")
  )

  broom::tidy(tttmp)


  tttmpz <- do_model_framing_lme4(tttmp)
  tttmpz$RE %>%
    tidyr::gather(
      key = "variable",
      val = "value",
      -groupFctr, -groupID
    )



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



