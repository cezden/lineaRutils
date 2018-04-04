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

  coef(model.fit2)
  rstanarm::posterior_interval(model.fit2)
  rstanarm::se(model.fit2)
  confint(model.fit2)
  sigma(model.fit2)

  #str(rstanarm::prior_summary(model.fit2))





})



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

