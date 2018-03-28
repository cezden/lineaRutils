#' @export
mixl_specification_preparse_single <- function(model.description.named){
  model.name <- names(model.description.named)
  assertthat::are_equal(length(model.name), 1)
  model.def <- model.description.named[[model.name]]

  innerf <- function(field, default = FALSE){
    val <- model.def[[field]]
    if (is.null(val)) {
      default
    } else {
      val
    }
  }
  extends.name <- innerf("extends", NA)
  is.extension <- !is.na(extends.name)
  is.virtual <- innerf("virtual", FALSE)


  data.frame(
    model.name = model.name,
    extends.name = extends.name,
    is.extension = is.extension,
    is.virtual = is.virtual,
    stringsAsFactors = FALSE
  )
}


mixl_specification_preparse_validate <- function(model.spec.df) {
  bad <- model.spec.df %>%
    dplyr::count(model.name) %>%
    dplyr::filter(n > 1)

  if (nrow(bad) > 0) {
    error_print_and_sig("Duplicated model names", bad)
  }

  bad <- model.spec.df %>%
    dplyr::filter(model.name == extends.name)
  if (nrow(bad) > 0) {
    error_print_and_sig("Model(s) extends itself", bad)
  }

  extensions.df <- model.spec.df %>%
    dplyr::filter(is.extension)

  if (nrow(extensions.df) > 0) {
    extensions.df <- extensions.df %>%
      dplyr::left_join(
        model.spec.df %>%
          dplyr::mutate(has.definition = TRUE) %>%
          dplyr::select(has.definition, extends.name = model.name),
        by = "extends.name"
      ) %>%
      dplyr::mutate(
        has.definition = ifelse(is.na(has.definition), FALSE, has.definition)
      )

    bad <- extensions.df %>%
      dplyr::filter(!has.definition)
    if (nrow(bad) > 0) {
      error_print_and_sig("Model extends unknown model", bad)
    }
  }

}

#' Model Set Preparsing
#' @export
mixl_specification_preparse <- function(raw_spec){
  ret <- lapply(
    names(raw_spec),
    function(model.name){
      mixl_specification_preparse_single(raw_spec[model.name])
    }) %>%
    dplyr::bind_rows()

  mixl_specification_preparse_validate(model.spec.df = ret)

  ret
}


#' Extends-aware Model Parsing Order
#' @export
mixl_specification_resolve_inner_preparse <- function(raw_spec, verbose = FALSE){
  spec.df <- mixl_specification_preparse(raw_spec) %>%
    dplyr::mutate(
      model.id = 1:n()
    )

  model_pool_resolve_parsing_order(
    models.stage1.df = spec.df,
    verbose = verbose
    )

}

