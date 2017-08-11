context("timeToRuin.scenario")

test_that("01 test that timeToRuin.scenario returns a list with known scenarios and result data", {
res = timeToRuin.scenario(spending=100,nper=3,mu=0.01,sigma=0.01,wealth=294,nScenarios=5, returnScenarios = TRUE,quantiles=c(0,0.25,0.5,0.75,1), seed =11082017)
  a=matrix(data=c(rep(294.000000,5),194.4219933,199.7748408,198.658656,191.528324,201.114507,98.3200702,99.8956320,98.499824,92.825205,103.841429,-0.6384581,0.3375682,0.106155,-4.911751,5.636063),ncol=5,byrow =TRUE)
  colnames(a)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5")
  rownames(a)=c("Time0","Time1","Time2","Time3")
  expect_identical(round(res$Scenarios,digits = 5),round(a,digits = 5))

  b=c(2,NA,NA,2,NA)
  names(b)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5")
  expect_identical(res$LastYearWithPositiveWealth,b)


  c=matrix(data=c(rep(294.000000,5),191.528, 194.422, 198.659, 199.775, 201.115,92.825,  98.320,  98.500,  99.896, 103.841,-4.912,  -0.638,   0.106,   0.338,   5.636),ncol=5,byrow =TRUE)
  colnames(c)=c("0%","25%","50%","75%","100%")
  rownames(c)= c("Time0","Time1","Time2","Time3")
  expect_identical(  round(res$Quantile_scenarios,digits = 3), c)

})


test_that("02 test that timeToRuin.scenario returns a list with no scenarios and known result data", {
  res = timeToRuin.scenario(spending=1000,nper=3,mu=1,sigma=1,wealth=1000,nScenarios=10, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =11082017)
  a=matrix(NA)
  expect_identical(round(res$Scenarios,digits = 5),round(a,digits = 5))

  b=c(1,NA,NA,0,NA,NA,1,0,1,NA)
  names(b)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5","Scenario6","Scenario7","Scenario8","Scenario9","Scenario10")
  expect_identical(res$LastYearWithPositiveWealth,b)


  c=matrix(data=c(rep(1000.000000,5),-570.1246,85.28934,1904.400,5450.471,9925.337,-7339.6194,-846.28832,3882.012, 16456.454, 54068.176,-7314.8643, -2198.39811,1247.439,72012.464,791950.302),ncol=5,byrow =TRUE)
  colnames(c)=c("0%","25%","50%","75%","100%")
  rownames(c)= c("Time0","Time1","Time2","Time3")
  expect_identical(  round(res$Quantile_scenarios,digits = 3), round(c,digits = 3))

})

test_that("02 test that timeToRuin.scenario returns a list with no scenarios and known result data with spending as ts", {
  res = timeToRuin.scenario(spending=ts(rep(1000,3)),nper=3,mu=1,sigma=1,wealth=1000,nScenarios=10, returnScenarios = FALSE,quantiles=c(0,0.25,0.5,0.75,1), seed =11082017)
  a=matrix(NA)
  expect_identical(round(res$Scenarios,digits = 5),round(a,digits = 5))

  b=c(1,NA,NA,0,NA,NA,1,0,1,NA)
  names(b)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5","Scenario6","Scenario7","Scenario8","Scenario9","Scenario10")
  expect_identical(res$LastYearWithPositiveWealth,b)


  c=matrix(data=c(rep(1000.000000,5),-570.1246,85.28934,1904.400,5450.471,9925.337,-7339.6194,-846.28832,3882.012, 16456.454, 54068.176,-7314.8643, -2198.39811,1247.439,72012.464,791950.302),ncol=5,byrow =TRUE)
  colnames(c)=c("0%","25%","50%","75%","100%")
  rownames(c)= c("Time0","Time1","Time2","Time3")
  expect_identical(  round(res$Quantile_scenarios,digits = 3), round(c,digits = 3))

})
