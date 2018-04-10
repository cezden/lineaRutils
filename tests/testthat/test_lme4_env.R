context("lme4 env wrap")

call_test_loc <- function(a, b, c){

  cc <- lme4_glmer_fit2(
    model.formula = a,
    model.data = b,
    model.params = c
  )

  list(
    model = cc
  )

}

verify_fit <- function(model.fit, test.model.formula, test.model.family){
  testthat::expect_is(model.fit, "glmerMod")

  obj.formula <- as.character(model.fit@call$formula)
  testthat::expect_equal(obj.formula, test.model.formula)

  obj.family <- model.fit@resp$family$family
  testthat::expect_equal(obj.family, test.model.family)

  model.summary <- summary(model.fit)
  testthat::expect_is(model.summary, "summary.merMod")
  testthat::expect_equal(model.summary$family, test.model.family)

  model.profile <- profile(model.fit, signames = FALSE)
  testthat::expect_is(model.profile, "thpr")
  confint(model.profile)

  summary_output <- testthat::capture_output_lines({
    print(summary(model.fit))
  })
  #print(summary_output)

  testthat::expect_lt(length(summary_output), 30)

}

test_that("classical fit 0",{

  model.fit0 <- lme4::glmer(
    cbind(incidence, size - incidence) ~ 1 + (1 | herd),
    data = lme4::cbpp,
    family = "binomial"
    )


  verify_fit(
    model.fit = model.fit0,
    test.model.formula = "cbind(incidence, size - incidence) ~ 1 + (1 | herd)",
    test.model.family = "binomial"
  )
  #print(summary(model.fit))

})


test_that("fitted model can be profiled in different environment 1",{
  test.model.data <- lme4::cbpp
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd)"
  model.fit <- lme4_glmer_fit_default(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = list()
    )
  verify_fit(
    model.fit = model.fit,
    test.model.formula = test.model.formula,
    test.model.family = "binomial"
  )

})

test_that("fitted model can be profiled in different environment 2",{
  test.model.data <- lme4::cbpp
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd)"
  test.model.params <- list(
    family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa")
  )
  model.fit <- lme4_glmer_fit2(
    model.formula = test.model.formula,
    model.data = test.model.data,
    model.params = test.model.params
  )
  verify_fit(
    model.fit = model.fit,
    test.model.formula = test.model.formula,
    test.model.family = "binomial"
  )

})

test_that("fitted model can be profiled in different environment 3",{
  test.model.data <- lme4::cbpp
  test.model.formula <- "cbind(incidence, size - incidence) ~ 1 + (1 | herd)"
  test.model.params <- list(
    family = binomial,
    control = lme4::glmerControl(optimizer = c("bobyqa", "Nelder_Mead"))
  )
  model.fit2 <- call_test_loc(
    a = test.model.formula,
    b = test.model.data,
    c = test.model.params
  )
  verify_fit(
    model.fit = model.fit2$model,
    test.model.formula = test.model.formula,
    test.model.family = "binomial"
    )

})

