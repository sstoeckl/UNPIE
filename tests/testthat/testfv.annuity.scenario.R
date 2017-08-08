context("fv.annuity.scenario")

test_that("01 test that fv.annuity.scenario returns a dataframe with known scenarios and result data", {
  res =fv.annuity.scenario(pmt=-1000,nper=4,mu=0.03,sigma=0.08,convRate=0.05,nScenario=5, returnScenarios = TRUE, quantiles=c(0,0.25,0.5,0.75,1),seed =1)

  a=matrix(data=c(rep(1000,5),2045.705,1964.990, 2005.584, 1863.142, 2111.274, 2971.692, 3105.350, 3332.354, 3100.675, 3323.301,4479.032, 4394.621, 4542.619, 4183.640, 4591.144),ncol=5,byrow =TRUE)
  colnames(a)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5")
  rownames(a)=c("Time1","Time2","Time3","Time4")
  expect_identical(round(res$Scenarios,digits = 3),a)

  b=c(223.952,219.731,227.131,209.182,229.557)
  names(b)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5")
  expect_identical(round(res$Lifelong_pension,digits = 3),b)

  c=c(209.182,219.731,223.952,227.131,229.557)
  names(c)=c("Scenario4", "Scenario2", "Scenario1", "Scenario3", "Scenario5")
  expect_identical(round(res$Lifelong_pension_sorted,digits = 3),c)

  d=matrix(data=c(rep(1000,5),1863.142,1964.990,2005.584,2045.705,2111.274,2971.692,3100.675,3105.350,3323.301,3332.354,4183.640,4394.621,4479.032,4542.619,4591.144),ncol=5,byrow =TRUE)
  colnames(d)=c("0%","25%","50%","75%","100%")
  rownames(d)= c("Time1","Time2","Time3","Time4")
  expect_identical(  round(res$Quantile_scenarios,digits = 3),d)

})


test_that("02 test that fv.annuity.scenario returns a dataframe without scenarios and known result data", {
  res =fv.annuity.scenario(pmt=-1000,nper=4,mu=0.03,sigma=0.08,convRate=0.05,nScenario=5, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1),seed =08082017)

  expect_identical(res$Scenarios,matrix(NA))

  a=c(180.528,222.217,218.045,182.567,193.033)
  names(a)=c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5")
  expect_identical(round(res$Lifelong_pension,digits = 3),a)

  b= c(180.528,182.567,193.033,218.045,222.217)
  names(b)=c("Scenario1", "Scenario4", "Scenario5", "Scenario3", "Scenario2")
  expect_identical(round(res$Lifelong_pension_sorted,digits = 3),b)

  c=matrix(data=c(rep(1000,5),1900.539,1981.499,1988.734,2042.475,2123.987,2574.704,2898.000,2951.083,3147.676,3169.414,3610.568,3651.349,3860.656,4360.892,4444.345),ncol=5,byrow =TRUE)
  colnames(c)=c("0%","25%","50%","75%","100%")
  rownames(c)= c("Time1","Time2","Time3","Time4")
  expect_identical(round(res$Quantile_scenarios,digits = 3),c)

})


test_that("03 test that fv.annuity.scenario returns true Lifelong_pension_sorted when pmt is of type ts", {
  res =fv.annuity.scenario(pmt=ts(rep(-1000,4)),nper=4,mu=0.03,sigma=0.08,convRate=0.05,nScenario=5, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1),seed =08082017)

  a=c(180.528,182.567,193.033,218.045,222.217)
  names(a)=c("Scenario1", "Scenario4", "Scenario5", "Scenario3", "Scenario2")
  expect_identical(round(res$Lifelong_pension_sorted,digits = 3),a)

  b=matrix(data=c(rep(1000,5),1900.539,1981.499,1988.734,2042.475,2123.987,2574.704,2898.000,2951.083,3147.676,3169.414,3610.568,3651.349,3860.656,4360.892,4444.345),ncol=5,byrow =TRUE)
  colnames(b)=c("0%","25%","50%","75%","100%")
  rownames(b)= c("Time1","Time2","Time3","Time4")
  expect_identical(round(res$Quantile_scenarios,digits = 3),b)

})

test_that("04 test that fv.annuity.scenario returns true Lifelong_pension_sorted when mu is of type ts", {
  res =fv.annuity.scenario(pmt=-1000,nper=4,mu=ts(rep(0.03,4)),sigma=0.08,convRate=0.05,nScenario=5, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1),seed =08082017)

  a=c(180.528,182.567,193.033,218.045,222.217)
  names(a)=c("Scenario1", "Scenario4", "Scenario5", "Scenario3", "Scenario2")
  expect_identical(round(res$Lifelong_pension_sorted,digits = 3),a)

  b=matrix(data=c(rep(1000,5),1900.539,1981.499,1988.734,2042.475,2123.987,2574.704,2898.000,2951.083,3147.676,3169.414,3610.568,3651.349,3860.656,4360.892,4444.345),ncol=5,byrow =TRUE)
  colnames(b)=c("0%","25%","50%","75%","100%")
  rownames(b)= c("Time1","Time2","Time3","Time4")
  expect_identical(round(res$Quantile_scenarios,digits = 3),b)

})

test_that("05 test that fv.annuity.scenario returns true Lifelong_pension_sorted when sigma is of type ts", {
  res =fv.annuity.scenario(pmt=-1000,nper=4,mu=0.03,sigma=ts(rep(0.08,4)),convRate=0.05,nScenario=5, returnScenarios = FALSE, quantiles=c(0,0.25,0.5,0.75,1),seed =08082017)

  a=c(180.528,182.567,193.033,218.045,222.217)
  names(a)=c("Scenario1", "Scenario4", "Scenario5", "Scenario3", "Scenario2")
  expect_identical(round(res$Lifelong_pension_sorted,digits = 3),a)

  b=matrix(data=c(rep(1000,5),1900.539,1981.499,1988.734,2042.475,2123.987,2574.704,2898.000,2951.083,3147.676,3169.414,3610.568,3651.349,3860.656,4360.892,4444.345),ncol=5,byrow =TRUE)
  colnames(b)=c("0%","25%","50%","75%","100%")
  rownames(b)= c("Time1","Time2","Time3","Time4")
  expect_identical(round(res$Quantile_scenarios,digits = 3),b)

})

