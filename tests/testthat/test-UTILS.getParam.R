library(RFate)
context(".getParam() function")

## INPUTS
test_that(".getParam gives error with missing data", {
  
  ## TEST params.lines : length > 0
  expect_error(.getParam(NA), "`params.lines` must contain a character value of length > 0")
  expect_error(.getParam(NULL), "`params.lines` must contain a character value of length > 0")
})

## INPUTS
test_that(".getParam gives error with wrong data", {
  
  ## TEST params.lines : length > 0
  expect_error(.getParam(1), "`params.lines` must contain a character value of length > 0")
  expect_error(.getParam(factor("a")), "`params.lines` must contain a character value of length > 0")
  expect_error(.getParam(factor(1)), "`params.lines` must contain a character value of length > 0")
  
  ## TEST params.lines : exist
  expect_error(.getParam(params.lines = "fake"), "`fake` does not exist")
  write.table(data.frame(1), file = "TEST_getParam.txt")
  
  ## TEST flag : length > 0
  expect_error(.getParam(params.lines = "TEST_getParam.txt")
               , "`flag` must contain a character value of length > 0")
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = NA)
               , "`flag` must contain a character value of length > 0")
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = NULL)
               , "`flag` must contain a character value of length > 0")
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = 1)
               , "`flag` must contain a character value of length > 0")
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = factor(1))
               , "`flag` must contain a character value of length > 0")
  
  ## TEST flag.split : correct value
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1")
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = NA)
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = NULL)
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = 1)
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = factor(1))
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "aa")
               , "`flag.split` must be either ` ` or `^--.*--$`", fixed = TRUE)
  
  ## TEST flag not found
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = " ")
               , "`flag` (PARAM1) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag` (--END_OF_FILE--) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  write.table(data.frame("--END_OF_FILE--", 12)
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  write.table(data.frame(c("--T--", "--END_OF_FILE--"), 12)
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  write.table(data.frame(c("--T--", "--END_OF_FILE--"))
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag` (PARAM1) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  write.table(data.frame(c("PARAM1", "--END_OF_FILE--"))
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  
  
  ## TEST flag.split not found
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$", is.num = FALSE)
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  write.table(data.frame(c("PARAM1", "--END_OF_FILE--"), 12)
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$")
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = "^--.*--$", is.num = FALSE)
               , "`flag.split` (^--.*--$) is not found within `params.lines` (TEST_getParam.txt)"
               , fixed = TRUE)
  
  
  ## TEST is.num : logical
  expect_error(.getParam(params.lines = "TEST_getParam.txt", flag = "PARAM1", flag.split = " ", is.num = "a")
               , "Wrong type of data!\n `is.num` must be logical", fixed = TRUE)
})



## OUTPUTS
test_that(".getParam gives correct output : 1 value, flag.split 1", {
  write.table(data.frame("PARAM1", 12)
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = " "
                                , is.num = FALSE)), 1)
  expect_equal(.getParam(params.lines = "TEST_getParam.txt"
                         , flag = "PARAM1"
                         , flag.split = " "), 12)
})


## OUTPUTS
test_that(".getParam gives correct output : 1 value, flag.split 2", {
  write.table(data.frame(c("--PARAM1--", 12, "--END_OF_FILE--"))
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = "^--.*--$")), 1)
  expect_equal(.getParam(params.lines = "TEST_getParam.txt"
                         , flag = "PARAM1"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE), "12")
})


## OUTPUTS
test_that(".getParam gives correct output : 2 values, flag.split 1", {
  write.table(data.frame("PARAM1", 12, 3)
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = " "
                                , is.num = FALSE)), 2)
  expect_equal(.getParam(params.lines = "TEST_getParam.txt"
                         , flag = "PARAM1"
                         , flag.split = " "), c(12,3))
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = " ")), 2)
})


## OUTPUTS
test_that(".getParam gives correct output : 2 values, flag.split 2", {  
  write.table(data.frame(c("--PARAM1--", 12, 3, "--END_OF_FILE--"))
              , file = "TEST_getParam.txt"
              , col.names = F, row.names = F, quote = F)
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = "^--.*--$"
                                , is.num = FALSE)), 2)
  expect_equal(.getParam(params.lines = "TEST_getParam.txt"
                         , flag = "PARAM1"
                         , flag.split = "^--.*--$"
                         , is.num = TRUE), c(12,3))
  expect_equal(.getParam(params.lines = "TEST_getParam.txt"
                         , flag = "PARAM1"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE), c("12","3"))
  expect_equal(length(.getParam(params.lines = "TEST_getParam.txt"
                                , flag = "PARAM1"
                                , flag.split = "^--.*--$")), 2)
})
