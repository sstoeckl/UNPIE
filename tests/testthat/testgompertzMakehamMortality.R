context("Test the Gompertz-Makeham Law of Mortality implementation")

test_that("1 test that Gompertz-Makeham Law of Mortality implementation runs", {
  x=65
  lambda=0
  m=82.3
  b=11.4
  rc=0.01
  res = gompertzMakehamMortality(x,lambda,m,b,rc)
  expect_equal(length(res),11)
})
