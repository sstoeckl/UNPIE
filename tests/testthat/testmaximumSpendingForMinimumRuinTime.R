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
  endScenarios =c(6731.035, 24353.217, 32183.962, 577.821, 0.000, 31369.033, 13081.150, 2989.629, 2862.348, 24574.368, 17901.909, 3184.029, 9706.088, 1297.878, 11153.039, 19700.602,0.000,4495.097,5108.639,19185.197)
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
  expect_equal(res$nr, c(27, 40, 49, 16, 11, 22, 21, 24, 19, 22, 24, 16, 22, 22, 43, 26, 13, 22, 17, 47))
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
  expect_equal(round(res$res$root,4), 535.9375)
})

test_that("4 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=16000
  minumumRuinTime = 20
  mu=0
  sigma=0
  nScenarios=1
  prob = 0.999999
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4),800.0000)
})

test_that("5 test annuity found by maximum spending for minimum ruintime with known seed (scenarios)", {
  wealth=16000
  minumumRuinTime = 20
  mu=0
  sigma=0
  nScenarios=1
  prob = 1
  seed =1
  res = maximumSpendingForMinimumRuinTime(wealth,minumumRuinTime,mu,sigma,nScenarios, prob, seed)
  expect_equal(round(res$res$root,4),800.0000)
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
  expect_equal(round(res$res$root,4),197.0786)
})

