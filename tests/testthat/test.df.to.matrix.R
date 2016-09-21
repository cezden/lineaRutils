context("Data frame to matrix conversion")

prepare.aqq <- function(){
  aqq <- airquality %>% dplyr::select(Ozone, Solar.R, Wind, Temp, Month, Day)
  aqq$Month <- factor(airquality$Month)
  aqq$Day <- factor(airquality$Day)
  aqq
}

verfy.mat.invariants <- function(df.res){
  df.m.trans <- df.res$mat.x.transform
  df.m <- df.res$mat.x
  expect_true(!is.null(df.m.trans))
  expect_true(!is.null(df.m))

  expect_equal(colnames(df.m), df.m.trans$res.col.name)
  expect_equal(seq(from = 1, length.out = nrow(df.m.trans)), df.m.trans$res.col.idx)
}

verify.conv.invariants <- function(df, df.res){
  df.m.trans <- df.res$mat.x.transform
  df.m <- df.res$mat.x

  expect_equal(nrow(df.m), nrow(df))
  expect_equal(ncol(df.m), nrow(df.m.trans))
  df.desc <- df.describe.basic(df)
  expect_equal(ncol(df.m), nrow(df.desc) - sum(df.desc$nlevels > 0) + sum(df.desc$nlevels))
  expect_equal(sum(df.desc$nlevels), sum(!is.na(df.m.trans$factor.level)))
}

verify.simple.as.matricable <- function(df, df.res){
  df.m.trans <- df.res$mat.x.transform
  df.m <- df.res$mat.x

  asm.df <- as.matrix(df)

  expect_equal(ncol(df.m), ncol(asm.df))
  expect_equal(nrow(df.m), nrow(asm.df))
  expect_equivalent(as.matrix(df.m), asm.df)
  expect_equal(colnames(df.m), colnames(asm.df))
  expect_equal(df.m.trans$org.col.name, colnames(df.m))
  expect_equal(df.m.trans$res.col.name, df.m.trans$org.col.name)

}

test_that("conversion with mixed factor and non-factor columns, factors last",{
  aqq <- prepare.aqq() %>% dplyr::select(Ozone, Solar.R, Wind, Temp, Month, Day)
  #aqq$Month <- factor(airquality$Month, ordered = TRUE)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  #expect_equal(ncol(aqq) + nlevels(aqq$Month) + nlevels(aqq$Day) - 2, ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)

})

test_that("conversion with mixed factor and non-factor columns, factors first",{
  aqq <- prepare.aqq() %>% dplyr::select(Month, Day, Ozone, Solar.R, Wind, Temp)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  #expect_equal(ncol(aqq) + nlevels(aqq$Month) + nlevels(aqq$Day) - 2, ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)

})

test_that("conversion with mixed factor and non-factor columns, factors between",{
  aqq <- prepare.aqq() %>% dplyr::select(Ozone, Month, Solar.R, Day, Wind, Temp)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  #expect_equal(ncol(aqq) + nlevels(aqq$Month) + nlevels(aqq$Day) - 2, ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)

})

test_that("conversion with mixed factor and non-factor columns, dense matrices",{
  aqq <- prepare.aqq() %>%
    dplyr::select(Ozone, Month, Solar.R, Wind, Temp) %>%
    dplyr::mutate(Solar.R2 = Solar.R, Wind2 = Wind, Temp2 = Temp)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  #expect_equal(ncol(aqq) + nlevels(aqq$Month) - 1, ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)

})


test_that("conversion with factor columns only",{
  aqq <- prepare.aqq() %>% dplyr::select(Month, Day)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  expect_equal(nlevels(aqq$Month) + nlevels(aqq$Day), ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)

})

test_that("conversion with single factor column",{
  aqq <- prepare.aqq() %>% dplyr::select(Month)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x

  verify.conv.invariants(aqq, aqq.res)
  #expect_equal(nlevels(aqq$Month), ncol(aqq.mat))
  verfy.mat.invariants(aqq.res)
})

test_that("conversion with non-factor columns only",{
  aqq <- prepare.aqq() %>% dplyr::select(Ozone, Solar.R)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x
  aqq.mat.true <- as.matrix(aqq)

  verify.conv.invariants(aqq, aqq.res)
  verfy.mat.invariants(aqq.res)
  verify.simple.as.matricable(aqq, aqq.res)
})

test_that("conversion with single non-factor column",{
  aqq <- prepare.aqq() %>% dplyr::select(Ozone)
  aqq.res <- df.as.matrix.dummy(aqq)
  aqq.mat.trans <- aqq.res$mat.x.transform
  aqq.mat <- aqq.res$mat.x
  aqq.mat.true <- as.matrix(aqq)

  verify.conv.invariants(aqq, aqq.res)
  verfy.mat.invariants(aqq.res)
  verify.simple.as.matricable(aqq, aqq.res)
})

test_that("conversion of empty data.frame",{
  empty.df <- data.frame()
  empty.res <- df.as.matrix.dummy(empty.df)
  empty.mat.trans <- empty.res$mat.x.transform
  empty.mat <- empty.res$mat.x
  empty.mat.true <- as.matrix(empty.df)

  verify.conv.invariants(empty.df, empty.res)
  verify.simple.as.matricable(empty.df, empty.res)
  expect_equal(nrow(empty.mat), 0)
  expect_equal(ncol(empty.mat), 0)
  expect_equal(as.matrix(empty.mat), empty.mat.true)
  expect_equal(colnames(empty.mat), colnames(empty.mat.true))
  verfy.mat.invariants(empty.res)
  dput(as.matrix(data.frame()))
})


