#' Validated model description
#'
#' @param model.def.named a model-level list with single model
#' @export
model_description <- function(model.def.named){

  model.name <- names(model.def.named)
  assertthat::are_equal(length(model.name), 1)

  model.def <- model.def.named[[model.name]]

  #parsing
  model.def.parts <- names(model.def)

  desc.elems.spec.df <- mixl_language_specification

  observed.spec.df <- data.frame(
    element.name = model.def.parts,
    is.observed = TRUE,
    stringsAsFactors = FALSE
  )

  is.virtual <- !is.null(model.def$virtual) & (model.def$virtual == TRUE)

  spec.validation.df <- desc.elems.spec.df %>%
    dplyr::full_join(
      observed.spec.df,
      by = "element.name"
    ) %>%
    dplyr::mutate(
      is.observed = ifelse(is.na(is.observed), FALSE, is.observed)
    )

  reqs <- if (is.virtual) {
    "is.required.abstract"
  } else {
    "is.required.instance"
  }

  spec.validation.df$is.required <- spec.validation.df[, reqs]

  spec.validation.df <- spec.validation.df %>%
    dplyr::mutate(
      is.fulfilled = !is.required | is.observed
    )

  spec.validation.fails.df <- spec.validation.df %>%
    dplyr::filter(!is.fulfilled)

  if (nrow(spec.validation.fails.df) > 0) {
    error_print_and_sig(
      error.msg = paste0("Incorrect definition of model [", model.name, "]"),
      x = spec.validation.fails.df
    )
  }

  if (!is.null(model.def$extends)) {
    good <- (model.def$extends != model.name[1])
    if (!good) {
      stop(paste("Model", model.name, "extends itself"))
    }
  }

  ret <- list()
  ret$model.name <- model.name
  ret$preformula <- mixl_parse_specification_formula(model.description = model.def)
  ret$final.definition <- model.def
  ret$obj.properties <- list(
    extends.name = extends.name,
    is.extension = is.extension,
    is.virtual = is.virtual
  )

  ret$has.proper.formula <- !is.null(ret$preformula)
  if (ret$has.proper.formula) {
    ret$variables <- ret$preformula$formula.data.variables
    ret$formula.str <- ret$preformula$formula.str
  }



  class(ret) <- "model_description"

  ret
}

get_model_name.model_description <- function(model.desc){
  model.desc$model.name
}

is_extension.model_description <- function(model.desc){
  model.desc$obj.properties$is.extension
}

get_parent_name.model_description <- function(model.desc){
  model.desc$obj.properties$extends.name
}
