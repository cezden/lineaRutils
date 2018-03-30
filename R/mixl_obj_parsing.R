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


model_pool_resolve_inner_extends <- function(model.def, parent.model){
  lang.elems <- mixl_language_specification %>%
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


model_pool_resolve_inner <- function(model.def, model.preparse.result, model.pool, verbose = FALSE){

  if (is_extension(model.def)) {
    ext.name <- get_parent_name(model.def)
    ext.def <- model.pool[[ext.name]]
    res.model <- model_pool_resolve_inner_extends(
      model.def = model.def,
      parent.model = ext.def
    )
    model.def.named <- list()
    model.def.named[[model.name]] <- res.model
  }
  if (model.preparse.result$is.extension) {
    ext.name <- model.preparse.result$extends.name

  }
  parsed.spec <- mixl_parse_specification(model.def.named)

  parsed.spec

}




#' @export
model_pool_resolve <- function(model.pool.preparsed, verbose = FALSE){
  new.model.pool <- list()

  parse.order.df <- get_parsing_order_df(model.pool.preparsed)

  for (rowid in 1:nrow(parse.order.df)) {
    model.pre.df <- parse.order.df[rowid, ]
    model.name <- model.pre.df$model.name
    model.predescription <- get_model(model.pool.preparsed, model.name = model.name)

    model.out <- model_pool_resolve_inner(
      model.predescription = model.predescription,
      model.preparse.result = model.pre.df,
      model.pool = new.model.pool,
      verbose = verbose
    )
    new.model.pool[[model.name]] <- model.out
  }

  #mixl_parse_specification

  new.model.pool
}

