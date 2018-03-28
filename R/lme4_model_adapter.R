lme4_glmer_fit_default <- function(model.formula, model.data, model.params){
  model.form <- as.formula(model.formula)
  mixmodel <- lme4::glmer(
    formula = model.form, data = model.data,
    family = binomial, control = lme4::glmerControl(optimizer = c("bobyqa", "Nelder_Mead")))
  mixmodel
}

#' @export
lme4_glmer_fit <- function(model.formula, model.data, model.params){
  # binding environment
  model.form <- as.formula(model.formula)
  model.params$data <- model.data
  model.params$formula <- model.form
  mixmodel <- do.call(what = lme4::glmer, args = model.params)
  mixmodel
}



