library(RFate)
context(".createParams() function")

## INPUTS
test_that(".createParams gives error with missing data", {
  expect_error(.createParams(NA), "`params.file` must contain a character value of length > 0")
  expect_error(.createParams(NULL), "`params.file` must contain a character value of length > 0")
})

## INPUTS
test_that(".createParams gives error with wrong data", {
  
  ## TEST params.file : length > 0
  expect_error(.createParams(1), "`params.file` must contain a character value of length > 0")
  expect_error(.createParams(factor("a")), "`params.file` must contain a character value of length > 0")
  expect_error(.createParams(factor(1)), "`params.file` must contain a character value of length > 0")
  
  ## TEST params.file : .txt
  expect_error(.createParams(params.file = "fake"), "`params.file` must be a file name with .txt extension")
  
  ## TEST params.file : exist
  expect_error(.createParams(params.file = "FAKE_dir/fake.txt"), "does not exist")
  
  ## TEST params.list : list
  expect_error(.createParams(params.file = "fake.txt"), "`params.list` must be a list")
  expect_error(.createParams(params.file = "fake.txt", params.list = NA), "`params.list` must be a list")
  expect_error(.createParams(params.file = "fake.txt", params.list = NULL), "`params.list` must be a list")
  expect_error(.createParams(params.file = "fake.txt", params.list = 1), "`params.list` must be a list")
  expect_error(.createParams(params.file = "fake.txt", params.list = "a"), "`params.list` must be a list")
  expect_error(.createParams(params.file = "fake.txt", params.list = factor(1)), "`params.list` must be a list")
  
  ## TEST params.list : list with non-null names
  expect_error(.createParams(params.file = "fake.txt", params.list = list()), "`params.list` must be a list with non-null names")
  expect_error(.createParams(params.file = "fake.txt", params.list = list(1,2)), "`params.list` must be a list with non-null names")
})


## OUTPUTS
test_that(".createParams gives correct output", {
  expect_message(.createParams(params.file = "fake.txt", params.list = list(A=1,B=2)), "has been successfully created !")
  expect_warning(.createParams(params.file = "fake.txt", params.list = list(A=1,B=2)), "already exists. It will be replaced.")
})
