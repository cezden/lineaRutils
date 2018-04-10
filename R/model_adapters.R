#' @export
rstanarm_fit <- function(x, y, ...) {
  UseMethod("rstanarm_fit")
}

#' @export
lme4_fit <- function(x, y, ...) {
  UseMethod("lme4_fit")
}


#' @export
lme4_glmer_formulator_parser <- function(mix_glForm){
  cnms <- mix_glForm$reTrms$cnms
  # a list of column names of the random effects according to the grouping factors
  cnms.re <- lapply(
    names(cnms),
    function(mix.comp.name){
      data.frame(
        term = cnms[[mix.comp.name]],
        grpvar = mix.comp.name,
        stringsAsFactors = FALSE
      )
    }
  ) %>% dplyr::bind_rows()

  Ztlist <- mix_glForm$reTrms$Ztlist
  # a list of column names of the random effects according to the grouping factors
  Ztlist.re <- lapply(
    names(Ztlist),
    function(mix.comp.name){
      data.frame(
        factor_value = rownames(Ztlist[[mix.comp.name]]),
        component = mix.comp.name,
        stringsAsFactors = FALSE
      )
    }
  ) %>% dplyr::bind_rows()

  list(
    cnms.re = cnms.re,
    Ztlist.re = Ztlist.re,
    effects.fe = colnames(mix_glForm$X)
  )
}


#' (MixL) Model structure specification
#'
#' @param model.formula (\code{chr}) the model formula
#' @param model.data (\code{data.frame}) the model data
#' @param model.params (\code{list}) the model structure description: \code{family}, \code{link}
#' @export
mixl_glmer_structure_spec <- function(model.formula, model.data, model.params){
  # binding environment
  model.form <- as.formula(model.formula)
  model.params$data <- model.data
  model.params$formula <- model.form
  mixmodel_formula <- do.call(what = lme4::glFormula, args = model.params)
  ret <- list(
    model.params = model.params,
    mixmodel.formula = mixmodel_formula,
    mixmodel.formula.p = lme4_glmer_formulator_parser(mixmodel_formula)
  )
  class(ret) <- 'mixl_glmer_structure_spec'

  ret
}







#' The model fitting environment configuration
#' @export
fitting_strategy <- function(model_output_dir = "./"){
  ret <- list(
    model_output_dir = model_output_dir,
    file_timed_format = "%Y%m%d_%H_%M_%S_CORENAME"
  )

  class(ret) <- 'fitting_strategy'
  ret
}



get_filepath <- function(x, y, ...) {
  UseMethod("get_filepath")
}

#' @export
get_filepath.fitting_strategy <- function(fit.stgy, model_core_name){

  timed.templ <- strftime(lubridate::now(tzone = "UTC"), format = fit.stgy$file_timed_format, tz = "GMT")
  fname.pre <- paste0(fit.stgy$model_output_dir, "/")
  fname.core <- paste0(model_core_name, ".RData")
  fname <- stringi::stri_replace_last_fixed(str = timed.templ, pattern = "CORENAME", replacement = fname.core)

  paste0(fname.pre, fname)
}


