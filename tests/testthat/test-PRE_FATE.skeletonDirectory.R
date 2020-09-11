library(RFate)
context("PRE_FATE.skeletonDirectory() function")

## INPUTS
test_that("PRE_FATE.skeletonDirectory gives message / warning / error with missing data", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  
  ## TEST name.simulation : length > 0
  expect_error(PRE_FATE.skeletonDirectory(NA), "`name.simulation` must contain a character value of length > 0")
  expect_error(PRE_FATE.skeletonDirectory(NULL), "`name.simulation` must contain a character value of length > 0")
  expect_error(PRE_FATE.skeletonDirectory(1), "`name.simulation` must contain a character value of length > 0")
  expect_error(PRE_FATE.skeletonDirectory(factor("a")), "`name.simulation` must contain a character value of length > 0")
  expect_error(PRE_FATE.skeletonDirectory(factor(1)), "`name.simulation` must contain a character value of length > 0")
})


## OUTPUTS
test_that("PRE_FATE.skeletonDirectory gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  expect_message(PRE_FATE.skeletonDirectory(), "Your directory tree for your FATE simulation")
  expect_warning(PRE_FATE.skeletonDirectory(), "Directory already exists!")
})
