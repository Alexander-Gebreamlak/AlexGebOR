

library(usethis)
library(testthat)
library(OR_95CI)

test_that("OR and CI output are correct", {
  expect_equal(OR_95CI(2, 0.5, 0.05, 2), "7.39 (2.77, 19.69)")
  expect_error(OR_95CI("a", 0.5, 0.05, 2), "All inputs must be numeric")
})

