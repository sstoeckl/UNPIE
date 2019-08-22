context("requiredSavingsForMinimumAnnuity: Finds the required savings for minimum annuity")

test_that("1 test required savings for minimum annuity", {
  nper=20
  mu=0.00
  sigma=0.02
  convRate=0.05
  nScenarios=1000
  minPayouy=10000
  prob = 0.1
  seed =1
  res = requiredSavingsForMinimumAnnuity(nper,mu,sigma,convRate,nScenarios,minPayouy, prob, seed,print=FALSE)
  expect_equal(9409.975, round(res,3))
})

test_that("2 test required savings for minimum annuity", {
  nper=30
  mu=0.10
  sigma=0.1
  convRate=0.05
  nScenarios=1000
  minPayouy=10000
  prob = 0.9
  seed =1
  res = requiredSavingsForMinimumAnnuity(nper,mu,sigma,convRate,nScenarios,minPayouy, prob, seed,print=FALSE)
  expect_equal(1830.5879, round(res,4))
})

test_that("3 test required savings for minimum annuity with plot", {
  nper=30
  mu=0.10
  sigma=0.1
  convRate=0.05
  nScenarios=5
  minPayouy=10000
  prob = 0.55
  seed =1
  res = requiredSavingsForMinimumAnnuity(nper,mu,sigma,convRate,nScenarios,minPayouy, prob, seed,print=TRUE)
  expect_equal(1202.9045, round(res,4))
})

test_that("4 test required savings for minimum annuity with scenarios", {
  nper=30
  mu=0.10
  sigma=0.1
  convRate=0.05
  nScenarios=5
  minPayouy=10000
  prob = 0.55
  seed =1
  res = requiredSavingsForMinimumAnnuity(nper,mu,sigma,convRate,nScenarios,minPayouy, prob, seed,print=FALSE, returnScenarios = TRUE)
  expect_equal(17743.7785, round(res$lifelong_pensions[[5]],4))
  expect_equal(354875.57, round(res$depot_scenariros[5,30],3))
  expect_equal(1202.9045, round(res$perodic_savings,4))
})
