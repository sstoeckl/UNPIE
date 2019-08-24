context("scenarioGenerator: Makes scenarios with rnorm. ")

test_that("1 test that known scenarios are generated", {
  res= scenarioGenerator(nper=5,nScenarios=5,mean=0,sd=1,seed =10082017)
mat = matrix(c(1,2.212659333,3.982395414,5.438243657,7.399122034,
               1,1.674084036,2.204050167,2.538824237,2.626506827,
               1,1.508132961,1.705232322,1.968962561,2.139869937,
               1,1.858926590,6.121019354,11.053543707,31.719455354,
               1,3.396168944,4.987866996,9.651276089,10.572522161),nrow=5,byrow = T)
  expect_equal(mat, round(res,8))
})
