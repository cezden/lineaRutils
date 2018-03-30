#' MixL Model PreDescription
#'
#' @param model.raw.description.named \code{list(model.name = list(...))}
#' @export
model_description_pre <- function(model.raw.description.named){

  model.name <- names(model.raw.description.named)
  assertthat::are_equal(length(model.name), 1)
  model.def <- model.raw.description.named[[model.name]]

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

  ret <- list()
  ret$model.name <- model.name
  ret$model.raw.def <- model.def
  ret$obj.properties <- list(
    extends.name = extends.name,
    is.extension = is.extension,
    is.virtual = is.virtual
  )

  class(ret) <- 'model_description_pre'
  ret
}

get_model_name <- function(x, y, ...) {
  UseMethod("get_model_name")
}

get_model_name.model_description_pre <- function(model.desc){
  model.desc$model.name
}

is_extension <- function(x, y, ...) {
  UseMethod("is_extension")
}

is_extension.model_description_pre <- function(model.desc){
  model.desc$obj.properties$is.extension
}

get_parent_name <- function(x, y, ...) {
  UseMethod("get_parent_name")
}

get_parent_name.model_description_pre <- function(model.desc){
  model.desc$obj.properties$extends.name
}


get_preparser_df <- function(x, y, ...) {
  UseMethod("get_preparser_df")
}

get_preparser_df.model_description_pre <- function(model.desc){
  obj.p <- model.desc$obj.properties
  data.frame(
    model.name = model.desc$model.name,
    extends.name = obj.p$extends.name,
    is.extension = obj.p$is.extension,
    is.virtual = obj.p$is.virtual,
    stringsAsFactors = FALSE
  )
}




#' @export
model_pool_preparsed <- function(raw.list, verbose = FALSE){
  ret <- list()
  ret$raw.list <- raw.list

  # preparsing
  models.stage1 <- model_pool_preparse_do(raw.list)
  models.stage1.df <- model_pool_preparse_df(models.stage1) %>%
    dplyr::mutate(
      model.id = 1:n()
    )

  # preparsing validation
  mixl_specification_preparse_validate(model.spec.df = models.stage1.df)

  ret$models.pre <- models.stage1
  ret$models.pre.df <- models.stage1.df

  # parsing order for "extends"
  models.stage2.df <- model_pool_resolve_parsing_order(
    models.stage1.df,
    verbose = verbose
    )

  ret$models.pre.parse.order <- models.stage2.df

  class(ret) <- 'model_pool_preparsed'
  ret
}

get_model <- function(x, y, ...) {
  UseMethod("get_model")
}

get_model.model_pool_preparsed <- function(model.pool.pre, model.name){
  model.pool.pre$models.pre[[model.name]]
}

get_parsing_order_df <- function(x, y, ...) {
  UseMethod("get_parsing_order_df")
}

get_parsing_order_df.model_pool_preparsed <- function(model.pool.pre) {
  model.pool.pre$models.pre.parse.order
}


get_model.model_pool_preparsed <- function(model.pool.pre, model.name){
  model.pool.pre$models.pre[[model.name]]
}


#' @export
model_pool <- function(model.pool.preparsed, verbose = FALSE){
  ret <- list()
  ret$raw.list <- raw.list

  ret$models.stage2 <- model_pool_resolve(
    parse.order.df = get_parsing_order_df(model.pool.preparsed),
    models.stage1.spec = models.stage1
  )


  class(ret) <- 'model_pool'
  ret
}




get_formula_str <- function(x, y, ...) {
  UseMethod("get_formula_str")
}

get_formula_str.model_description <- function(x){

}

