context("MixL parsing")

verify_formula_processing <- function(form, parsed, vars, max.int.order = 1, has.icept = TRUE, has.resp = TRUE){
  cat(paste("testing",form, "\n" ))
  testthat::expect_equal(
    sort(parsed$formula.data.variables),
    sort(vars)
    )
  form.prop <- parsed$formula.properties

  testthat::expect_false(is.null(form.prop))
  testthat::expect_is(form.prop, "data.frame")
  testthat::expect_setequal(colnames(form.prop), c("max.interaction.order", "has.intercept", "has.response"))
  testthat::expect_true(nrow(form.prop) == 1)

  testthat::expect_true(form.prop$max.interaction.order == max.int.order)
  testthat::expect_true(form.prop$has.intercept == has.icept)
  testthat::expect_true(form.prop$has.response == has.resp)
}

parse_form_char <- function(form) mixl_parse_formula(as.formula(form), verbose = FALSE)

test_that("formula processing",{

  test_cases <- list(
    list(
      form = "a ~ 0", vars = c("a"), max.int.order = 0, has.icept = FALSE
    ),
    list(
      form = "a ~ 1", vars = c("a"), max.int.order = 0
    ),
    list(
      form = "a ~ b", vars = c("a", "b")
    ),
    list(
      form = "a ~ (1 | b)", vars = c("a", "b")
    ),
    list(
      form = "a ~ (1 | b) + 0", vars = c("a", "b"), has.icept = FALSE
    ),
    list(
      form = "I(log(a)) ~ (1 | b) + 0", vars = c("a", "b"), has.icept = FALSE
    ),
    list(
      form = "a ~ (1 | b) + (1 | c) + (1 | d)", vars = c("a", "b", "c", "d")
    ),
    list(
      form = "a ~ (1 | b:c)", vars = c("a", "b", "c"), max.int.order = 2
    ),
    list(
      form = "a ~ (1 | b:c) + 0", vars = c("a", "b", "c"), max.int.order = 2, has.icept = FALSE
    ),
    list(
      form = "a ~ (1 | b:c) + (1 | b) + (1 | c)", vars = c("a", "b", "c"), max.int.order = 2
    ),
    list(
      form = "a ~ (1 | b/c) + 0", vars = c("a", "b", "c"), max.int.order = 2, has.icept = FALSE
    ),
    list(
      form = "a ~ (b/c) + 0", vars = c("a", "b", "c"), max.int.order = 2, has.icept = FALSE
    )
    #,
    #list(
    #  form = "cbind(a,b) ~ (1 | c) + (1 | d)", vars = c("a", "b", "c", "d")
    #)

  )

  lapply(
    test_cases,
    function(tc){
      call.args <- tc
      call.args$parsed <- parse_form_char(tc$form)
      do.call(verify_formula_processing, call.args)

    }
    )



  #mixl_parse_formula(mout, verbose = TRUE)



  #mixl_parse_formula(as.formula("a ~ ."))

})
