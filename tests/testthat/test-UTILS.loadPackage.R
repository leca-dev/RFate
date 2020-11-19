library(RFate)
context(".loadPackage() function")

## INPUTS
test_that(".loadPackage gives message / warning / error with missing data", {
  expect_error(.loadPackage(), "No data given!\n (missing `package.name`)", fixed = TRUE)
  expect_error(.loadPackage(NA), "No data given!\n (missing `package.name`)", fixed = TRUE)
  expect_error(.loadPackage(NULL), "No data given!\n (missing `package.name`)", fixed = TRUE)
  
  ## TEST package.name : character
  expect_error(.loadPackage(1), "`package.name` must contain a character value")
  expect_error(.loadPackage(factor("a")), "`package.name` must contain a character value")
  expect_error(.loadPackage(factor(1)), "`package.name` must contain a character value")
})

