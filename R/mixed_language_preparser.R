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
    dplyr::filter(is.extension) %>%
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

    now.closed.model.names <- (spec.df.status %>%
                                 dplyr::filter(is.closed))$model.name

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
    dplyr::arrange(parsing.step)

}

