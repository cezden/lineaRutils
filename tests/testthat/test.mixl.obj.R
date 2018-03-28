context("MixL objects")

test_that("model_description_pre creation",{

  raw.data.named <- list(
    model0 = list(
      extends = "model1"
    )
  )

  test.desc <- model_description_pre(raw.data.named)
  testthat::expect_is(test.desc, "model_description_pre")
  testthat::expect_equal(test.desc$model.name, "model0")
  testthat::expect_equal(test.desc$obj.properties$extends.name, "model1")

})

test_that("model_description_pre properties",{

  raw.data.named <- list(
    model0 = list(
      extends = "model1"
    )
  )

  test.desc <- model_description_pre(raw.data.named)

  testthat::expect_equal(get_model_name(test.desc), "model0")

  test.desc.df <- get_preparser_df(test.desc)

  testthat::expect_is(test.desc.df, "data.frame")
  preparser_df.expected.cols <- c("model.name", "extends.name", "is.extension", "is.virtual")

  testthat::expect_setequal(names(test.desc.df), preparser_df.expected.cols)
  testthat::expect_equal(nrow(test.desc.df), 1)

  testthat::expect_equal(test.desc.df$model.name, test.desc$model.name)
  testthat::expect_equal(test.desc.df$extends.name, "model1")
  testthat::expect_equal(test.desc.df$is.virtual, FALSE)
  testthat::expect_equal(test.desc.df$is.extension, TRUE)


})


test_that("model_pool_preparse_do",{

  raw.data.named <- list(
    model0 = list(
      extends = "model1"
    )
  )

  test.desc <- model_pool_preparse_do(raw.data.named)


  testthat::expect_is(test.desc, "list")
  testthat::expect_equal(length(test.desc), 1)

  testthat::expect_equal(names(test.desc), c("model0"))
  testthat::expect_is(test.desc$model0, "model_description_pre")
  testthat::expect_equal(test.desc$model0$model.name, "model0")

})
