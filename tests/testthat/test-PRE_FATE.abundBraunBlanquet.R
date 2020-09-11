library(RFate)
context("PRE_FATE.abundBraunBlanquet() function")

## INPUTS
test_that("PRE_FATE.abundBraunBlanquet gives error with missing data", {
  expect_error(PRE_FATE.abundBraunBlanquet()
               , "No data given!\n (missing `abund` information)", fixed = TRUE)
  expect_error(PRE_FATE.abundBraunBlanquet(NA)
               , "No data given!\n (missing `abund` information)", fixed = TRUE)
  expect_error(PRE_FATE.abundBraunBlanquet(NULL)
               , "No data given!\n (missing `abund` information)", fixed = TRUE)
})

## INPUT
test_that("PRE_FATE.abundBraunBlanquet gives error with wrong data : abund", {

  ## TEST abund : correct values
  expect_error(PRE_FATE.abundBraunBlanquet(TRUE)
               , "`abund` must be either `NA`, `NA`, `+`, `r`, `1`, `2`, `3`, `4` or `5`"
               , fixed = TRUE)
  expect_error(PRE_FATE.abundBraunBlanquet(-1)
               , "`abund` must be either `NA`, `NA`, `+`, `r`, `1`, `2`, `3`, `4` or `5`"
               , fixed = TRUE)
  expect_error(PRE_FATE.abundBraunBlanquet("a")
               , "`abund` must be either `NA`, `NA`, `+`, `r`, `1`, `2`, `3`, `4` or `5`"
               , fixed = TRUE)
  expect_error(PRE_FATE.abundBraunBlanquet(c("1", "6"))
               , "`abund` must be either `NA`, `NA`, `+`, `r`, `1`, `2`, `3`, `4` or `5`"
               , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.abundBraunBlanquet of BB values give right results", {
  expect_equal(PRE_FATE.abundBraunBlanquet("r"), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("+"), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("1"), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet("2"), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet("3"), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("4"), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet("5"), 87.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("r")), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("+")), 0.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("1")), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("2")), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("3")), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("4")), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor("5")), 87.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(1), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet(2), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet(3), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(4), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(5), 87.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor(1)), 3)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor(2)), 15)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor(3)), 37.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor(4)), 62.5)
  expect_equal(PRE_FATE.abundBraunBlanquet(factor(5)), 87.5)
})

## OUTPUTS
test_that("PRE_FATE.abundBraunBlanquet of missing give NA", {
  expect_equal(PRE_FATE.abundBraunBlanquet("NA"), as.numeric(NA))
})

## OUTPUTS
test_that("PRE_FATE.abundBraunBlanquet does not modify length", {
  expect_equal(length(PRE_FATE.abundBraunBlanquet("NA")), 1)
  expect_equal(length(PRE_FATE.abundBraunBlanquet("1")), 1)
  expect_equal(length(PRE_FATE.abundBraunBlanquet(c("1", NA))), 2)
  expect_equal(length(PRE_FATE.abundBraunBlanquet(c("1", "NA"))), 2)
  expect_equal(length(PRE_FATE.abundBraunBlanquet(c(NA, NA))), 2)
})

## OUTPUTS
test_that("PRE_FATE.abundBraunBlanquet gives numeric output", {
  expect_output(str(PRE_FATE.abundBraunBlanquet(1)), "num")
  expect_output(str(PRE_FATE.abundBraunBlanquet("1")), "num")
  expect_output(str(PRE_FATE.abundBraunBlanquet(c(1, NA))), "num")
})

