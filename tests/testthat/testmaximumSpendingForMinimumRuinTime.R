context("maximumSpendingForMinimumRuinTime: Calculates scenarios of future value of annuity payments (fv) with stochastic returns")

test_that("1 test final scenarios of maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=14000
  minumumRuinTime = 16
  mu=0.03
  sigma=0.2
  nScenarios=20
  prob = 0.9
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  endScenarios = c(7224.432, 25784.773,33775.417, 717.387,0,33039.937,13909.525,3310.917,3167.904,25975.294,18969.691,3490.610,10501.794,1480.161,11783.630,20740.086,0,4828.274,5588.127, 20247.550)
  expect_equal(endScenarios, round(res$scenarios[,16],3))
})

test_that("2 test survival time of maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=14000
  minumumRuinTime = 16
  mu=0.03
  sigma=0.2
  nScenarios=20
  prob = 0.9
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(res$nr, c(21,21,21,16,11,21,21,21,19,21,21,21,21,21,21,21,13,21,17,21))
})

test_that("3 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=14000
  minumumRuinTime = 16
  mu=0.03
  sigma=0.2
  nScenarios=20
  prob = 0.9
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4), 543.6932)
})

test_that("4 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=10000
  minumumRuinTime = 11
  mu=0
  sigma=0
  nScenarios=1
  prob = 0.999999
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4),1000.0000)
})

test_that("5 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=10000
  minumumRuinTime = 11
  mu=0
  sigma=0
  nScenarios=1
  prob = 1
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4),1000.0000)
})

test_that("6 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=10000
  minumumRuinTime = 30
  mu=0.1
  sigma=0.4
  nScenarios=1000
  prob = 0.4
  seed =100
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4),201.0661)
})

