context("scenarioGenerator: Makes scenarios with rnorm. ")

test_that("1 test that known scenarios are generated", {
  res= scenarioGenerator(nper=5,nScenarios=5,mean=0,sd=1,seed =10082017)
mat = matrix(c(1,2.7560294,3.3269062,5.7568954,7.7236779,1,1.5189166,1.2317870,-0.5579708,-1.3241450,1,0.1059297,0.2051675,0.1548864,0.1584736,1,5.2036642,8.9458060,29.0158370,52.7035560,1,1.1818604,3.6784437,-3.0495517,2.3035519),nrow=5,byrow = T)
  expect_equal(mat, round(res,7))
})

test_that("2 test that known scenarios are generated", {
  res= scenarioGenerator(nper=5,nScenarios=5,mean=1,sd=1,seed =10082017)
  mat = matrix(c(1,4.756029,12.875225,39.446322,102.230643,1,3.518917,8.132090,7.159140,25.938593,1, 2.105930,7.237718,15.114238,34.357079, 1,7.203664,23.189574,106.135115,325.449752,1,3.181860,14.676070,7.401145,3.660799),nrow=5,byrow = T)
  expect_equal(mat, round(res,7))
})
