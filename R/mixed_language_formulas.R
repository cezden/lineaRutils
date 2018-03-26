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


  formula.LHS <- model.description$response
  formula.RHS.pre <- model.description$components

  if (any(is.null(formula.LHS), is.null(formula.RHS.pre))) {
    return(NULL)
  }

  formula.RHS <- paste(formula.RHS.pre, collapse = " + ")
  formula.str <- paste(formula.LHS, "~", formula.RHS)
  formula.obj <- as.formula(formula.str)
  formula.prop <- mixl_parse_formula(formula.obj)

  elems.1 <- list(
    formula.str.LHS = formula.LHS,
    formula.str.RHS = formula.RHS,
    formula.str = formula.str
  )

  c(elems.1, formula.prop)
}
