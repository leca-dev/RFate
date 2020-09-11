library(RFate)
context(".getCutoff() function")

## INPUTS
test_that(".getCutoff gives error with missing data", {
  
  ## TEST Obs : correct values
  expect_error(.getCutoff(NA), "Wrong type of data!\n `Obs` must be either `0` or `1`")
  expect_error(.getCutoff(NULL), "Wrong type of data!\n `Obs` must be either `0` or `1`")
  
  ## TEST Fit : correct values
  expect_error(.getCutoff(0), "Wrong type of data!\n `Fit` must contain values between `0` and `1`")
  expect_error(.getCutoff(0, NA), "Wrong type of data!\n `Fit` must contain values between `0` and `1`")
  expect_error(.getCutoff(0, NULL), "Wrong type of data!\n `Fit` must contain values between `0` and `1`")
  
})


## OUTPUTS
test_that(".getCutoff gives correct output", {
  expect_equal(.getCutoff(0, 0), NA)
  expect_equal(.getCutoff(0, 1), NA)
  
  expect_equal(.getCutoff(sample(c(0, 1), 100, replace = T), 0), NA)
  expect_equal(length(.getCutoff(sample(c(0, 1), 100, replace = T), sample(c(0, 1), 100, replace = T))), 3)
})
