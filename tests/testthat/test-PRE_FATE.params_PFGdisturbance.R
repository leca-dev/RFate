library(RFate)
context("PRE_FATE.params_PFGdisturbance() function")

## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with missing data", {
  expect_error(PRE_FATE.params_PFGdisturbance()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
})

## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGdisturbance(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
})

## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : mat.PFG.dist", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.tol : data.frame
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation")
               , "`mat.PFG.tol` must be a data.frame")
  
  ## TEST mat.PFG.dist : data.frame
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = NA)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = "")
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = 1)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = factor(1))
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = matrix(1))
               , "`mat.PFG.dist` must be a data.frame")
  
  ## TEST mat.PFG.dist : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame())
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(1))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(1,2,3,4))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist : correct names of columns
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.dist` must be `PFG`, `type`, `maturity`, `longevity` and `age_above_150cm`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.dist$PFG : different values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = c(2,2), maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = NA, type = 2, maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,NA), type = c(2,2), maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$PFG : length > 0
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = "", type = 1, maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.dist$type : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = NA, maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = 2, maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "", maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,2), type = c("H",NA), maturity = 1
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  
  
  ## TEST mat.PFG.dist$maturity : numeric values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = NA
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = "a"
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = factor(1)
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$maturity : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = c(3,NA)
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$maturity : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 2
                                                                          , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain values equal or inferior to `mat.PFG.dist$longevity`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist$longevity : numeric values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = NA, age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = "a", age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = factor(1), age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$longevity : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 1
                                                                          , longevity = c(3,NA), age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$age_above_150cm : numeric values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = 1, age_above_150cm = NA))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = 1, age_above_150cm = "a"))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                          , longevity = 1, age_above_150cm = factor(1)))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$age_above_150cm : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 1
                                                                          , longevity = 1, age_above_150cm = c(3,NA)))
               , "`mat.PFG.dist$age_above_150cm` must not contain NA values", fixed = TRUE)
  
})

## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : mat.PFG.tol", {
  
  ## TEST mat.PFG.tol : data.frame
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = NA)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = "")
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = 1)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = matrix(1))
               , "`mat.PFG.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.tol : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame())
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(1))
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol : correct names of columns
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(1,2,3))
               , "Column names of `mat.PFG.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$nameDist : length > 0
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = "", PFG = 1, strategy_tol = 1))
               , "`mat.PFG.tol$nameDist` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$PFG : length > 0
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "", strategy_tol = 1))
               , "`mat.PFG.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = NA, strategy_tol = 1))
               , "`mat.PFG.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.tol$PFG : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(PFG = "B", type = "H", maturity = 1
                                                                          , longevity = 1 , age_above_150cm = 1)
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = 1))
               , "`mat.PFG.tol$PFG` must be either `H`, `C`, `P` or `B`", fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$breakAge : numeric values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = NA, resproutAge = 1
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = "a", resproutAge = 1
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = factor(1), resproutAge = 1
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$breakAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$breakAge : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = c(1,NA), resproutAge = 1
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$breakAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$resproutAge : numeric values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = 1, resproutAge = NA
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = 1, resproutAge = "a"
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = 1, resproutAge = factor(1)
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$resproutAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$resproutAge : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , breakAge = 1, resproutAge = c(1,NA)
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$resproutAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$responseStage : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = c(1,NA)
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$responseStage` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$responseStage : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1.5
                                                                         , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.tol$responseStage` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$killedIndiv : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = c(1,NA), resproutIndiv = 1))
               , "`mat.PFG.tol$killedIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$killedIndiv : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = -1, resproutIndiv = 1))
               , "`mat.PFG.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = 1.5, resproutIndiv = 1))
               , "`mat.PFG.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$resproutIndiv : no NA values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = 1, resproutIndiv = c(1,NA)))
               , "`mat.PFG.tol$resproutIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$resproutIndiv : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = 1, resproutIndiv = -1))
               , "`mat.PFG.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                         , killedIndiv = 1, resproutIndiv = 1.5))
               , "`mat.PFG.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$strategy_tol : correct values
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = 1))
               , "`mat.PFG.tol$strategy_tol` must be either `indifferent`, `mowing_herbs`, `mowing_trees`, `grazing_herbs_1`, `grazing_herbs_2`, `grazing_herbs_3`, `grazing_trees_1`, `grazing_trees_2` or `grazing_trees_3`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = NA))
               , "`mat.PFG.tol$strategy_tol` must be either `indifferent`, `mowing_herbs`, `mowing_trees`, `grazing_herbs_1`, `grazing_herbs_2`, `grazing_herbs_3`, `grazing_trees_1`, `grazing_trees_2` or `grazing_trees_3`"
               , fixed = TRUE)
  
})




## OUTPUTS
test_that(paste0("PRE_FATE.params_PFGdisturbance gives correct output : "
                 , "BREAK_AGES scenario 0, RESPR_AGES scenario 0, FATES scenario 2"), {
                   if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
                   PRE_FATE.skeletonDirectory()
                   expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent"))
                                  , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_1.txt has been successfully created !")
                   expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent"))
                                  , "Missing data! The `BREAK_AGE` parameter has not been set. Please check.")
                   expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent"))
                                  , "Missing data! The `RESPR_AGE` parameter has not been set. Please check.")
                   expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent")
                                                                 , opt.folder.name = "")
                                  , "already exists. It will be replaced.")
                   expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent")
                                                                 , opt.folder.name = NA)
                                  , "As `opt.folder.name` does not contain character value, it will be ignored")
                   expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                                 , mat.PFG.dist = NULL
                                                                 , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                            , PFG = 1
                                                                                            , strategy_tol = "indifferent")
                                                                 , opt.folder.name = "TEST")
                                  , "The parameter file FATE_simulation/DATA/PFGS/DIST/TEST/DIST_1.txt has been successfully created !")
                 })


## OUTPUTS
test_that(paste0("PRE_FATE.params_PFGdisturbance gives correct output : "
                 , "BREAK_AGES scenario 1, RESPR_AGES scenario 1, FATES scenario 2")
          , {
            if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
            PRE_FATE.skeletonDirectory()
            expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                          , mat.PFG.dist = data.frame(PFG = 1
                                                                                      , type = "H"
                                                                                      , maturity = 4
                                                                                      , longevity = 5
                                                                                      , age_above_150cm = 10000)
                                                          , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                     , PFG = 1
                                                                                     , strategy_tol = "indifferent"))
                           , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_1.txt has been successfully created !")
          })


## OUTPUTS
test_that(paste0("PRE_FATE.params_PFGdisturbance gives correct output : "
                 , "BREAK_AGES scenario 2, RESPR_AGES scenario 2, FATES scenario 2")
          , {
            if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
            PRE_FATE.skeletonDirectory()
            expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                          , mat.PFG.dist = NULL
                                                          , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                     , PFG = 1
                                                                                     , strategy_tol = "indifferent"
                                                                                     , responseStage = c(1, 2)
                                                                                     , breakAge = 2
                                                                                     , resproutAge = 1))
                           , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_1.txt has been successfully created !")
          })


## OUTPUTS
test_that(paste0("PRE_FATE.params_PFGdisturbance gives correct output : "
                 , "BREAK_AGES scenario 2, RESPR_AGES scenario 2, FATES scenario 1")
          , {
            if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
            PRE_FATE.skeletonDirectory()
            expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                          , mat.PFG.dist = NULL
                                                          , mat.PFG.tol = data.frame(nameDist = "MOW"
                                                                                     , PFG = 1
                                                                                     , responseStage = c(1, 2)
                                                                                     , breakAge = 2
                                                                                     , resproutAge = 1
                                                                                     , killedIndiv = 5
                                                                                     , resproutIndiv = 4))
                           , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_1.txt has been successfully created !")
          })

