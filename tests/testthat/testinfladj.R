context("infladj: Inflationadjustment of payment(s)")

test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = ts(rep(-1000,30),start = 2000)
  nper=30
  infl = 0.02
  res = infladj(fv,infl,nper)-infladj(fv,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = ts(rep(-1000,30),start = 2000)
  infl1=0.02
  infl2=ts(rep(0.02,30),start = 2000)
  nper = 30
  res = infladj(fv,infl1,nper)-infladj(fv,infl2,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("3 test that start of a timeseries is set correct with no timeseries as input", {
  fv = -555
  infl = 0.02
  nper = 30
  res = infladj(fv,infl,nper)-infladj(fv,infl,nper)
  expect_identical(res,ts(rep(0,30),start =1))
})

test_that("4 test that start of a timeseries is set correct with no timeseries as input.", {
  fv = -555
  infl = 0.02
  nper = 30
  res = infladj(fv,infl,nper)-infladj(fv,infl,nper)
  expect_identical(res,ts(rep(0,30),start =1))
})

test_that("5 no ts input", {
  A = infladj(fv=1000,inflation=0, nper=10)
  expect_equal(A,ts(rep(1000,10)))
})

