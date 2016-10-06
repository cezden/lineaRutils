context("glmnet coefficients processing")

verify.glmnet.process.output.results.invariants <- function(glmnet.res, model.out, x.vars.count){
  glmnet.coef <- coef(glmnet.res) %>% as.matrix()

  steps.count <- ncol(glmnet.coef)
  vars.count <- nrow(glmnet.coef)

  steps.min <- min(model.out$steps$step)
  steps.max <- max(model.out$steps$step)
  # step starts with >=1
  expect_gte(steps.min, 1)
  # step does not exceed path length
  expect_lte(steps.max, steps.count)
  # number of fitted coeffs <= number of vars + intercept
  expect_lte(vars.count, x.vars.count + 1)

  #for each step from model.out$coeffs there exist a step in model.out$steps
  test.steps.in.coeffs.only <- base::setdiff(
    unique(model.out$coeffs$step),
    unique(model.out$steps$step)
    )
  expect_length(test.steps.in.coeffs.only, 0)


  #unique steps & coefficients

  steps.nrow.test <- model.out$steps %>% dplyr::distinct() %>% nrow()
  steps.nrow <- model.out$steps %>% nrow()

  expect_equal(steps.nrow.test, steps.nrow)

  coeffs.nrow.test <- model.out$coeffs %>% dplyr::distinct() %>% nrow()
  coeffs.nrow <- model.out$coeffs %>% nrow()

  expect_equal(coeffs.nrow.test, coeffs.nrow)

}

verify.glmnet.process.output.invariants <- function(glmnet.res, nonzero.coeffs.only, x.vars.count){
  model.out.fin <- glmnet.process.output(
    glmnet.res = glmnet.res,
    nonzero.coeffs.only = nonzero.coeffs.only,
    structural.filter.type = "final"
  )
  model.out.init <- glmnet.process.output(
    glmnet.res = glmnet.res,
    nonzero.coeffs.only = nonzero.coeffs.only,
    structural.filter.type = "initial"
  )

  verify.glmnet.process.output.results.invariants(glmnet.res, model.out.fin, x.vars.count)
  verify.glmnet.process.output.results.invariants(glmnet.res, model.out.init, x.vars.count)

  # number of subpaths is the same
  expect_equal(model.out.init$steps %>% nrow(), model.out.fin$steps %>% nrow())

}

test_that("LASSO",{
  data(longley)
  x <- as.matrix(longley[,1:6])
  y <- as.matrix(longley[,7])

  model <- glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 1, intercept = FALSE)
  verify.glmnet.process.output.invariants(glmnet.res = model, nonzero.coeffs.only = TRUE, x.vars.count = ncol(x))

  model <- glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 1, intercept = TRUE)
  verify.glmnet.process.output.invariants(glmnet.res = model, nonzero.coeffs.only = TRUE, x.vars.count = ncol(x))

})

test_that("alpha = 0.5",{
  data(longley)
  x <- as.matrix(longley[,1:6])
  y <- as.matrix(longley[,7])

  model <- glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 0.5, intercept = FALSE)
  verify.glmnet.process.output.invariants(glmnet.res = model, nonzero.coeffs.only = TRUE, x.vars.count = ncol(x))

  model <- glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 0.5, intercept = TRUE)
  verify.glmnet.process.output.invariants(glmnet.res = model, nonzero.coeffs.only = TRUE, x.vars.count = ncol(x))

})

test_that("structural.changes.col.matrix", {
  data(longley)
  x <- as.matrix(longley[,1:6])
  y <- as.matrix(longley[,7])
  model <- glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = 0.5, intercept = TRUE)

  a.mat <- coef(model)
  b.mat <- a.mat %>% as.matrix()
  a.tst <- structural.changes.col.matrix(a.mat, initial.substeps = TRUE, final.substeps = TRUE)
  b.tst <- structural.changes.col.matrix(b.mat, initial.substeps = TRUE, final.substeps = TRUE)
  expect_equal(a.tst, b.tst)
})
