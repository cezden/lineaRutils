
verbose_f <- function(f, verbose = FALSE){
  if (verbose) {
    f()
  }
}

verbose_print <- function(x, title = NULL, verbose = FALSE){
  verbose_f(
    function(){
      if (!is.null(title)) {
        title <- c(title, "")
      }
      cat(paste(title, collapse = "\n"))
      print(x)
    },
    verbose = verbose
  )
}

error_print_and_sig <- function(error.msg, x = NULL, title = NULL){
  if (is.null(title)) {
    title <- error.msg
  }
  if (!is.null(x)) {
    verbose_print(x = x, title = title, verbose = TRUE)
  }
  stop(error.msg)
}


mixl_parse_formula_internal <- function(frm){
  frm.vars <- all.vars(frm)
  frm.terms <- terms.formula(frm)
  frm.terms.att <- attributes(frm.terms)

  formula.properties <- data.frame(
    # in case of a ~ 1 frm.terms.att$order is empty
    max.interaction.order = max(c(0, frm.terms.att$order)),
    has.intercept = frm.terms.att$intercept,
    has.response = frm.terms.att$response
  )

  list(
    formula.data.variables = frm.vars,
    formula.properties = formula.properties
  )
}


#' MixLang formula information
#'
#' @param frm R formula object
#' @param verbose should the function be verbose
#' @export
mixl_parse_formula <- function(frm, verbose = FALSE){

  frm.rhs <- formula.tools::rhs(frm)
  frm.lhs <- formula.tools::lhs(frm)

  frm.int.rand <- frm %>%
    lme4::expandDoubleVerts() %>%
    lme4::subbars()
  frm.int.fixed <- frm
  formula.tools::rhs(frm.int.fixed) <- lme4::nobars(frm.rhs)

  verbose_print(frm.int.rand, title = "Random part", verbose = verbose)
  verbose_print(frm.int.fixed, title = "Fixed part", verbose = verbose)

  pars.rand <- mixl_parse_formula_internal(frm.int.rand)
  pars.fixed <- mixl_parse_formula_internal(frm.int.fixed)

  frm.vars <- unique(c(
    pars.rand$formula.data.variables, pars.fixed$formula.data.variables))

  fp.rand <- pars.rand$formula.properties
  fp.fixed <- pars.fixed$formula.properties

  formula.properties <- data.frame(
    max.interaction.order = max(c(
      0,
      fp.rand$max.interaction.order,
      fp.fixed$max.interaction.order)),
    has.intercept = max(c(0, fp.rand$has.intercept, fp.fixed$has.intercept)),
    has.response = fp.fixed$has.response
  )

  list(
    formula.data.variables = frm.vars,
    formula.properties = formula.properties
  )
}



#' @export
mixl_parse_specification_formula <- function(model.description){

  formula.LHS <- "cbind(successes, failures) ~"
  formula.RHS <- paste(model.description$components, collapse = " + ")
  formula.str <- paste(formula.LHS, formula.RHS)
  formula.obj <- as.formula(formula.str)
  formula.prop <- mixl_parse_formula(formula.obj)

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
mixl_parse_specification <- function(model.description.named){

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

  vret$preformula <- mixl_parse_specification_formula(model.description = model.def)
  #vret$


  vret
}

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



#' @export
mixl_specification_resolve_inner <- function(raw_spec, verbose = FALSE){
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



#' @export
parser_read_specification <- function(fpath){
  if (is.na(fpath)) {
    fname <- "model_defs_basic.yml"
    fname <- "model_defs_ext1.yml"
    fpath <- paste0("/home/cde/R/x86_64-pc-linux-gnu-library/3.4/FunneleR/extdata/", fname)
  }

  file.spec <- yaml::read_yaml(file = fpath, fileEncoding = "UTF-8")

  raw_spec <- file.spec

  mixl_specification_preparse(file.spec)
  mixl_specification_resolve_inner(file.spec, verbose = FALSE)

  names(file.spec)

  file.spec.tmp <- "model0"

  names(file.spec[file.spec.tmp])

  model.description <- file.spec[file.spec.tmp]
  model.description.named <- file.spec[[file.spec.tmp]]
  if (model.description.named[['virtual']]) {
    print("gfds")
  }




  mixl_parse_specification_formula(file.spec[[file.spec.tmp]])

  form.spec <- mixl_parse_specification_formula(file.spec[[file.spec.tmp]])




  frm.terms <- terms.formula(formula.obj)

  mixl_parse_formula(formula.obj)

  mixl_parse_formula(as.formula("."))
  mixl_parse_formula(as.formula("a ~ ."))
  mixl_parse_formula(as.formula("a ~ b"))
  mixl_parse_formula(as.formula("a ~ b"), verbose = TRUE)
  mixl_parse_formula(as.formula("a ~ (1 | b)"))
  mixl_parse_formula(as.formula("a ~ (1 | b) + 0"))
  mixl_parse_formula(as.formula("I(log(a)) ~ (1 | b) + 0"))
  mixl_parse_formula(as.formula("a ~ (1 | b) + (1 | c) + (1 | d) "))
  mixl_parse_formula(as.formula("a ~ (1 | b) + (1 | c) + (1 | d) + (1 | d)"))
  mixl_parse_formula(as.formula("a ~ (1 | b:c) + 0"))
  mixl_parse_formula(as.formula("a ~ (1 | b/c) + 0"))
  mixl_parse_formula(as.formula("a ~ (b/c) + 0"))
  mixl_parse_formula(as.formula("cbind(a,b) ~ (1 | c) + (1 | d)"))


  all.vars(formula.obj)




  frm <- as.formula(mixl_parse_specification(file.spec[[1]]))

  str(frm)



}
