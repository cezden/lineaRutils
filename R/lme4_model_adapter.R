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
lme4_glmer_fit_set <- function(model.desc.set, model.data, verbose = FALSE){

  models.cnt <- length(model.desc.set)
  models <- lapply(
    names(model.desc.set),
    function(model.name){
      model.desc <- model.desc.set[[model.name]]

      model.form <- model.desc
      print(model.form)
      model.out <- lme4_glmer_fit(
        model.formula = model.form,
        model.data = model.data
      )
      save(model.out, file = paste0(model.name, ".RData"))
      print(summary(model.out))
      model.out
    })
  names(models) <- names(model.formulas)
  models
}



