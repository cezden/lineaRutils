model_pool_preparse_do <- function(raw.list){
  out.list <- lapply(
    names(raw.list),
    function(model.name){
      model.obj <- model_description_pre(raw.list[model.name])
      #nest_in_list(field_name = model.name, value = model.obj)
      model.obj
    }
  )

  names(out.list) <- names(raw.list)

  out.list
}

model_pool_preparse_df <- function(models.stage1){
  lapply(
    names(models.stage1),
    function(model.name){
      get_preparser_df(models.stage1[[model.name]])
    }
  ) %>%
    dplyr::bind_rows()
}

#' Parsing order
#'
#' @param models.stage1.df \code{data.frame}
#' @param verbose should the function be verbose
model_pool_resolve_parsing_order <- function(models.stage1.df, verbose = FALSE){
  spec.df <- models.stage1.df

  current.step <- 1

  spec.df.status <- spec.df %>%
    dplyr::mutate(
      is.closed = !is.extension,
      closed.step = ifelse(is.closed, current.step, -1)
    )
  verbose_print(spec.df.status, title = "Initial status table", verbose = verbose)
  n.df <- nrow(spec.df)
  n.df.closed <- sum(spec.df.status$is.closed)

  if (n.df.closed == n.df) {
    return(spec.df.status)
  }
  if (n.df.closed == 0) {
    stop("BAD: empty or cycle")
  }

  for (it in 1:(n.df - n.df.closed)) {
    current.step <- current.step + 1

    # continuation conditions
    if (all(spec.df.status$is.closed)) {
      break
    }

    now.closed.model.names <- (
      spec.df.status %>%
        dplyr::filter(is.closed)
    )$model.name

    spec.df.status <- spec.df.status %>%
      dplyr::mutate(
        now.closeable = !is.closed & (extends.name %in% now.closed.model.names),
        is.closed = ifelse(now.closeable, TRUE, is.closed),
        closed.step = ifelse(now.closeable, current.step, closed.step)
      )
    verbose_print(
      spec.df.status,
      title = paste("Status table during step", current.step),
      verbose = verbose)

    if (!any(spec.df.status$now.closeable)) {
      stop("BAD: empty or cycle")
    }
  } #for

  spec.df.status %>%
    dplyr::rename(
      parsing.step = closed.step
    ) %>%
    dplyr::select(
      -is.closed, -now.closeable
    ) %>%
    dplyr::arrange(parsing.step, model.id)

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


model_pool_resolve_inner <- function(model.predescription.named, model.preparse.result, model.pool, verbose = FALSE){
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
model_pool_resolve <- function(model.pool.preparsed, verbose = FALSE){
  new.model.pool <- list()

  parse.order.df <- get_parsing_order_df(model.pool.preparsed)

  for (rowid in 1:nrow(parse.order.df)) {
    model.preparse.result <- parse.order.df[rowid, ]
    model.name <- model.preparse.result$model.name
    model.predescription <- get_model(model.pool.preparsed, model.name = model.name)

    model.out <- model_pool_resolve_inner(
      model.predescription = model.predescription,
      model.preparse.result = model.preparse.result,
      model.pool = new.model.pool,
      verbose = verbose
    )
    new.model.pool[[model.name]] <- model.out
  }

  #mixl_parse_specification

  new.model.pool
}

