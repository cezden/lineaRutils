context("Model postprocessing")

test_that("lme4",{

  testthat::expect_true(TRUE)

})


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

  model.fit2.sum <- summary(model.fit2, pars = "varying")

  str(model.fit2.sum)


  model.fit2.sum.df <- model.fit2.sum %>% as.data.frame()

  zz1 <- fitted(model.fit2)

  rstanarm::ranef(model.fit2)
  rstanarm::fixef(model.fit2)

  coef(model.fit2)
  rstanarm::posterior_interval(model.fit2)
  rstanarm::se(model.fit2)
  confint(model.fit2)
  sigma(model.fit2)

  #str(rstanarm::prior_summary(model.fit2))





})
