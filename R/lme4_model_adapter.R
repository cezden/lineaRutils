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

#' @export
lme4_glmer_fit_set <- function(model.desc.set, model.data, model.fit.params.default, fitting.strategy = fitting_strategy(), verbose = FALSE){

  models <- lapply(
    get_model_names(model.desc.set),
    function(model.name){
      a_model <- get_model(model.desc.set, model.name)

      model.form <- get_formula_str(a_model)
      print(model.form)

      model.params <- get_fit_params(a_model, model.fit.params.default = model.fit.params.default)

      model.out <- lme4_glmer_fit(
        model.formula = model.form,
        model.data = model.data,
        model.params = model.params
      )

      fpath <- get_filepath(fitting.strategy, model_core_name = model.name)
      save(model.out, file = fpath)
      print(summary(model.out))

      list(
        model.name = model.name,
        file.path = fpath
      )
    })

  models
}



