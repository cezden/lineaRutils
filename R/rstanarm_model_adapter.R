#' @export
rstanarm_glmer_fit <- function(model.formula, model.data, model.params){
  # binding environment
  model.form <- as.formula(model.formula)
  model.params$data <- model.data
  model.params$formula <- model.form
  mixmodel <- do.call(what = rstanarm::stan_glmer, args = model.params)
  mixmodel
}
