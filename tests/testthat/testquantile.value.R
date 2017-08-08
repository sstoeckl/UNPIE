context("quantile.values")

test_that("test that quantile.value returns the true quantile values", {
  res =quantile.values(values=c(0:100), quantiles=c(0,0.25,0.5,0.75,1))

  expect_identical(res,quantile(c(0,25,50,75,100)))
})
