# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(AlexGebOR)

test_that("OR and CI output are correct", {
  expect_equal(OR_95CI(2, 0.5, 0.05, 2), "7.39 (2.77, 19.69)")
  expect_error(OR_95CI("a", 0.5, 0.05, 2), "All inputs must be numeric")
})
