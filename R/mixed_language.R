

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

mixl_specification_resolve <- function(raw_spec, verbose = FALSE){
  model.pool <- list()

  parse.order <- mixl_specification_resolve_inner(raw_spec = raw_spec, verbose = verbose)

  #mixl_parse_specification


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
