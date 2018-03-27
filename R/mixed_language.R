#' @export
mixl_language_specification <- function() {
  desc.elems.spec.strs <- c(
    "element.name,is.required.instance,is.required.abstract,scope.dsl,extends.strategy",
    "extends,FALSE,FALSE,TRUE,child_only",
    "virtual,FALSE,FALSE,TRUE,child_only",
    "components,TRUE,FALSE,TRUE,set_sum",
    "response,TRUE,TRUE,TRUE,child_parent",
    "family,TRUE,TRUE,FALSE,child_parent"
  )

  readr::read_csv(paste(desc.elems.spec.strs, collapse = "\n"))

}


#' Model description parser
#'
#' @param model.description a model-level list with single model
#' @export
mixl_parse_specification <- function(model.description.named){

  model.name <- names(model.description.named)
  assertthat::are_equal(length(model.name), 1)

  model.def <- model.description.named[[model.name]]

  model.def.parts <- names(model.def)

  desc.elems.spec.df <- mixl_language_specification()

  observed.spec.df <- data.frame(
    element.name = model.def.parts,
    is.observed = TRUE,
    stringsAsFactors = FALSE
  )

  is.virtual <- !is.null(model.def$virtual)

  spec.validation.df <- desc.elems.spec.df %>%
    dplyr::full_join(
      observed.spec.df,
      by = "element.name"
    ) %>%
    dplyr::mutate(
      is.observed = ifelse(is.na(is.observed), FALSE, is.observed)
    )

  if (is.virtual) {
    spec.validation.df$is.required <- spec.validation.df$is.required.abstract
  } else {
    spec.validation.df$is.required <- spec.validation.df$is.required.instance
  }

  spec.validation.df <- spec.validation.df %>%
    dplyr::mutate(
      is.fulfilled = !is.required | is.observed
    )

  spec.validation.fails.df <- spec.validation.df %>%
    dplyr::filter(!is.fulfilled)

  if (nrow(spec.validation.fails.df) > 0) {
    print(model.name)
    print(spec.validation.fails.df)
    stop("BAD")
  }

  if (!is.null(model.def$extends)) {
    good <- (model.def$extends != model.name[1])
    if (!good) {
      stop(paste("Model", model.name, "extends itself"))
    }
  }

  vret <- list()

  vret$preformula <- mixl_parse_specification_formula(model.description = model.def)
  vret$final.definition <- model.def
  if (!is.null(vret$preformula)) {
    vret$variables <- vret$preformula$formula.data.variables
    vret$formula.str <- vret$preformula$formula.str
  }

  #vret$


  vret
}

mixl_specification_resolve_inner_extends <- function(model.def, parent.model){
  lang.elems <- mixl_language_specification() %>%
    dplyr::filter(
      extends.strategy %in% c('set_sum', 'child_parent')
    )
  model.def.ext <- model.def
  defined.elems <- names(model.def)
  extension.def <- parent.model$final.definition
  for (it in 1:nrow(lang.elems)) {
    elem <- lang.elems[it, ]
    elem.name <- elem$element.name
    if (elem$extends.strategy == "set_sum") {
      model.def.ext[[elem.name]] <- c(extension.def[[elem.name]], model.def[[elem.name]])
    }
    if (elem$extends.strategy == "child_parent") {
      if (!(elem.name %in% defined.elems)) {
        model.def.ext[[elem.name]] <- extension.def[[elem.name]]
      }
    }

  }

  model.def.ext
}

mixl_specification_resolve_inner <- function(model.predescription.named, model.preparse.result, model.pool, verbose = FALSE){
  model.name <- names(model.predescription.named)
  assertthat::are_equal(length(model.name), 1)

  model.def <- model.predescription.named[[model.name]]
  model.def.named <- model.predescription.named
  if (model.preparse.result$is.extension) {
    ext.name <- model.preparse.result$extends.name
    ext.def <- model.pool[[ext.name]]
    res.model <- mixl_specification_resolve_inner_extends(model.def = model.def, parent.model = ext.def)
    model.def.named <- list()
    model.def.named[[model.name]] <- res.model

  }
  parsed.spec <- mixl_parse_specification(model.def.named)

  parsed.spec

}

#' @export
mixl_specification_resolve <- function(raw_spec, verbose = FALSE){
  model.pool <- list()

  parse.order <- mixl_specification_resolve_inner_preparse(raw_spec = raw_spec, verbose = verbose)
  for (rowid in 1:nrow(parse.order)) {
    model.preparse.result <- parse.order[rowid, ]
    model.name <- model.preparse.result$model.name
    model.predescription.named <- raw_spec[model.preparse.result$model.name]

    model.out <- mixl_specification_resolve_inner(
      model.predescription.named = model.predescription.named,
      model.preparse.result = model.preparse.result,
      model.pool = model.pool,
      verbose = verbose
    )
    model.pool[[model.name]] <- model.out
  }

  #mixl_parse_specification

  model.pool
}

#' MixLang Model Parser
#'
#' Function reads and parses model specification files
#'
#' @param fpath.vec (\code{character()}) vector of file names
#' @param verbose should the function be verbose
#' @export
mixl_specification_read_and_parse <- function(fpath.vec, verbose = FALSE){

  raw.spec <- list()

  for (fpath in fpath.vec) {
    file.spec <- yaml::read_yaml(file = fpath, fileEncoding = "UTF-8")
    raw.spec <- c(raw.spec, file.spec)
  }

  model.pool <- mixl_specification_resolve(raw_spec = raw.spec, verbose = verbose)

  model.pool
}

