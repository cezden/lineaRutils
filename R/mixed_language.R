verbose_print <- function(x, verbose = FALSE){
  if (verbose) {
    print(x)
  }
}


#' Formula information
#' @param frm R formula object
#' @export
parser_parse_formula <- function(frm, verbose = FALSE){
  frm.vars <- all.vars(frm)
  if ("." %in% frm.vars) {
    stop("BAD")
  }
  verbose_print(frm.vars, verbose)
  frm.terms <- terms.formula(frm)
  frm.terms.att <- attributes(frm.terms)
  verbose_print(frm.terms, verbose)

  formula.properties <- data.frame(
    max.interaction.order = max(frm.terms.att$order),
    has.intercept = frm.terms.att$intercept,
    has.response = frm.terms.att$response
  )
  formula.properties %>% verbose_print(verbose = verbose)

  list(
    formula.data.variables = frm.vars,
    formula.properties = formula.properties
  )
}



#' @export
the_model_fit_parse_specification_formula <- function(model.description){

  formula.LHS <- "cbind(successes, failures) ~"
  formula.RHS <- paste(model.description$components, collapse = " + ")
  formula.str <- paste(formula.LHS, formula.RHS)
  formula.obj <- as.formula(formula.str)
  formula.prop <- parser_parse_formula(formula.obj)

  elems.1 <- list(
    formula.str.LHS = formula.LHS,
    formula.str.RHS = formula.RHS,
    formula.str = formula.str
  )

  c(elems.1, formula.prop)
}





#' Model description parser
#'
#' @param model.description a model-level list with single model
#' @export
the_model_fit_parse_specification <- function(model.description.named){

  model.name <- names(model.description.named)
  assertthat::are_equal(length(model.name), 1)

  model.def <- model.description.named[[model.name]]

  model.def.parts <- names(model.def)

  desc.elems.spec.strs <- c(
    "element.name,is.required,scope.dsl",
    "extends,FALSE,TRUE",
    "virtual,FALSE,TRUE",
    "components,TRUE,TRUE",
    "family,FALSE,FALSE"
  )

  desc.elems.spec.df <- readr::read_csv(paste(desc.elems.spec.strs, collapse = "\n"))

  observed.spec.df <- data.frame(
    element.name = model.def.parts,
    is.observed = TRUE,
    stringsAsFactors = FALSE
  )

  spec.validation.df <- desc.elems.spec.df %>%
    dplyr::full_join(
      observed.spec.df,
      by = "element.name"
    ) %>%
    dplyr::mutate(
      is.observed = ifelse(is.na(is.observed), FALSE, is.observed)
    ) %>%
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
    assertthat::assert_that(model.def$extends != model.name, paste("Model", model.name, "extends itself"))
  }

  vret <- list()

  vret$preformula <- the_model_fit_parse_specification_formula(model.description = model.def)
  #vret$


  vret
}

#' @export
the_model_specification_preparse_single <- function(model.description.named){
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


#' @export
the_model_specification_preparse <- function(raw_spec){
  ret <- lapply(
    names(raw_spec),
    function(model.name){
      the_model_specification_preparse_single(raw_spec[model.name])
    }) %>%
    dplyr::bind_rows()
  bad <- ret %>%
    dplyr::count(model.name) %>%
    dplyr::filter(n > 1)
  if (nrow(bad) > 0) {
    print(bad)
    stop("Duplicated model names")
  }

  ret
}



#' @export
the_model_specification_resolve_inner <- function(raw_spec){
  spec.df <- the_model_specification_preparse(raw_spec) %>%
    dplyr::mutate(
      model.id = 1:n()
    )

  current.step <- 1

  spec.df.status <- spec.df %>%
    dplyr::mutate(
      is.closed = !is.extension,
      closed.step = 1
    )
  n.df <- nrow(spec.df)
  n.df.closed <- sum(spec.df.status$is.closed)

  if (n.df.closed == n.df) {
    return(spec.df.status)
  }
  if (n.df.closed == 0) {
    stop("BAD: empty or cycle")
  }

  for (it in 1:(n.df - n.df.closed)) {
    prev.n.df.closed <- n.df.closed
    current.step <- current.step + 1

    now.closed.idx <- spec.df.status$is.closed
    now.closed.model.names <- spec.df.status$model.name[now.closed.idx]
    n.now.closed <- length(now.closed.model.names)
    if (prev.n.df.closed == n.now.closed) {
      stop("BAD")
    }

    now.closeable <-

    break
  }

  for (i in 1:(nrow(spec.df.open) + 1)) {

  }

}



#' @export
the_model_fit_read_specification <- function(fpath = NA){
  if (is.na(fpath)) {
    fpath <- system.file("extdata", "model_defs_basic.yml", package = "FunneleR")
    fpath <- system.file("extdata", "model_defs_ext1.yml", package = "FunneleR")
  }

  file.spec <- yaml::read_yaml(file = fpath, fileEncoding = "UTF-8")

  raw_spec <- file.spec

  the_model_specification_preparse(file.spec)

  names(file.spec)

  file.spec.tmp <- "model.simple0"

  names(file.spec[file.spec.tmp])

  model.description <- file.spec[file.spec.tmp]
  model.description.named <- file.spec[[file.spec.tmp]]
  if (model.description.named[['virtual']]) {
    print("gfds")
  }




  the_model_fit_parse_specification_formula(file.spec[[file.spec.tmp]])

  form.spec <- the_model_fit_parse_specification_formula(file.spec[[file.spec.tmp]])




  frm.terms <- terms.formula(formula.obj)

  parser_parse_formula(formula.obj)

  parser_parse_formula(as.formula("."))
  parser_parse_formula(as.formula("a ~ ."))
  parser_parse_formula(as.formula("a ~ b"))
  parser_parse_formula(as.formula("a ~ b"), verbose = TRUE)
  parser_parse_formula(as.formula("a ~ (1 | b)"))
  parser_parse_formula(as.formula("a ~ (1 | b) + 0"))
  parser_parse_formula(as.formula("I(log(a)) ~ (1 | b) + 0"))
  parser_parse_formula(as.formula("a ~ (1 | b) + (1 | c) + (1 | d) "))
  parser_parse_formula(as.formula("a ~ (1 | b) + (1 | c) + (1 | d) + (1 | d)"))
  parser_parse_formula(as.formula("a ~ (1 | b:c) + 0"))
  parser_parse_formula(as.formula("a ~ (1 | b/c) + 0"))
  parser_parse_formula(as.formula("a ~ (b/c) + 0"))
  parser_parse_formula(as.formula("cbind(a,b) ~ (1 | c) + (1 | d)"))


  all.vars(formula.obj)




  frm <- as.formula(the_model_fit_parse_specification(file.spec[[1]]))

  str(frm)



}
