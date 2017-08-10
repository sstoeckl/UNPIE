context("fixLimit: Fixed the requested limit, such that is matches the step size of possible solutions. ")

test_that("1 test that no adjustment is needed", {
  nScenarios = 10
  prob = 0.5
  res = fixLimit(nScenarios,prob)
  expect_equal(0.5, res)
})

test_that("2 test that adjustment is needed", {
  nScenarios = 10
  prob = 0.55
  res = fixLimit(nScenarios,prob)
  expect_equal(0.6, res)
})


test_that("3 test that adjustment is needed", {
  nScenarios = 10
  prob = 0.95
  res = fixLimit(nScenarios,prob)
  expect_equal(1, res)
})

test_that("4 test that adjustment is needed", {
  nScenarios = 33
  prob = 0.90
  res = fixLimit(nScenarios,prob)
  expect_equal(0.9090909, res)
})
