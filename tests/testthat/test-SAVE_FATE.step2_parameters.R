library(RFate)
context("SAVE_FATE.step2_parameters() function")

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with missing values", {
  expect_error(SAVE_FATE.step2_parameters()
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step2_parameters(NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step2_parameters(NULL)
               , "`name.dataset` must contain a character value of length > 0")
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong type of data : name.dataset", {
  expect_error(SAVE_FATE.step2_parameters(name.dataset = NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = 1)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = factor("a"))
               , "`name.dataset` must contain a character value of length > 0")
})



## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : name.simulation", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = 1)
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = "a")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  
  dir.create("FATE_simulation")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL")
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : strata.limits", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST strata.limits : integer
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = NA)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = "a")
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = factor(1))
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = -1)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.succ", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.succ : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = "")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be a data.frame")
  
  ## TEST mat.PFG.succ : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, (max_abundance), (potential_fecundity), (immature_size), (is_alien), (flammability))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, (max_abundance), (potential_fecundity), (immature_size), (is_alien), (flammability))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.succ : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1,2,3,4,5,6))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1,2,3,4,5,6,7))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1,2,3,4,5,6,7,8))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(1,2,3,4,5,6,7,8,9))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, hop = 1))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = c(2,2)
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = NA, type = 2
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,NA), type = c(2,2)
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = "", type = NA
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$type : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = NA
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = 2
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = ""
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = c(2,NA)
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$height : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = NA, maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = "a", maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = factor(1), maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$height : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = c(3,NA), maturity = 4
                                                                      , longevity = 10))
               , "`mat.PFG.succ$height` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$maturity : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = NA
                                                                      , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = "a"
                                                                      , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = factor(1)
                                                                      , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$maturity : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = c(4,NA)
                                                                      , longevity = 10))
               , "`mat.PFG.succ$maturity` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$longevity : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = NA))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = "a"))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = factor(1)))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$longevity : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = c(10,NA)))
               , "`mat.PFG.succ$longevity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$maturity : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 2))
               , "`mat.PFG.succ$maturity` must contain values equal or inferior to `mat.PFG.succ$longevity`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$max_abundance : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, max_abundance = c(10,NA)))
               , "`mat.PFG.succ$max_abundance` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$max_abundance : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, max_abundance = "a"))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, max_abundance = 1.5))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, max_abundance = 10))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$potential_fecundity : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, potential_fecundity = NA))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, potential_fecundity = "a"))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, potential_fecundity = factor(1)))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$potential_fecundity : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, potential_fecundity = c(10,NA)))
               , "`mat.PFG.succ$potential_fecundity` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$immature_size : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = c(10,NA)))
               , "`mat.PFG.succ$immature_size` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$immature_size : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = "a"))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = 1.5))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = 11))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  
  ## TEST mat.PFG.succ$is_alien : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, is_alien = c(10,NA)))
               , "`mat.PFG.succ$is_alien` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$is_alien : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, is_alien = "a"))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, is_alien = 1.5))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, is_alien = 11))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$flammability : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, flammability = NA))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, flammability = "a"))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, flammability = factor(1)))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$flammability : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, flammability = c(10,NA)))
               , "`mat.PFG.succ$flammability` must not contain NA values", fixed = TRUE)
})



## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.light", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.light : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = NA)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = NULL)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = "")
               , "`mat.PFG.light` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = 1)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = factor(1))
               , "`mat.PFG.light` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = matrix(1))
               , "`mat.PFG.light` must be a data.frame")
  
  ## TEST mat.PFG.light : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame())
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(1))
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(1,2,3,4,5,6,7))
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(1,2))
               , "Column names of `mat.PFG.light` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)` and `(light_need)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.light` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)` and `(light_need)`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, type = c(2,2)))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = NA, type = 2))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,NA), type = c(2,2)))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.light$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = "", type = 1))
               , "`mat.PFG.light$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.light$type : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, type = NA))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, type = 2))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, type = ""))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), type = c("H",NA)))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  ## TEST mat.PFG.light$light_need : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), type = "H", light_need = c(3,NA)))
               , "`mat.PFG.light$light_need` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$light_need : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H", light_need = 1.5))
               , "`mat.PFG.light$light_need` must be either `0`, `1`, `2`, `3`, `4` or `5`", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_low : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = c(3,NA)
                                                                       , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_low : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = -1
                                                                       , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1.5
                                                                       , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  
  ## TEST mat.PFG.light$active_germ_medium : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                       , active_germ_medium = c(3,NA), active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_medium : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                       , active_germ_medium = -1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                       , active_germ_medium = 1.5, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  
  ## TEST mat.PFG.light$active_germ_high : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                       , active_germ_medium = 1, active_germ_high = c(3,NA)))
               , "`mat.PFG.light$active_germ_high` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_high : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                       , active_germ_medium = 1, active_germ_high = -1))
               , "`mat.PFG.light$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                       , active_germ_medium = 1, active_germ_high = 1.5))
               , "`mat.PFG.light$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  ## TEST mat.PFG.light$strategy_ag : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = NA))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = 2))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = ""))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = c(1,2), strategy_ag = c("indifferent",NA)))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.light.tol", {
  
  ## TEST mat.PFG.light.tol : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = NA)
               , "`mat.PFG.light.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = "")
               , "`mat.PFG.light.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = 1)
               , "`mat.PFG.light.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = matrix(1))
               , "`mat.PFG.light.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.light.tol : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame())
               , "`mat.PFG.light.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(1))
               , "`mat.PFG.light.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light.tol : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(1,2))
               , "Column names of `mat.PFG.light.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.light.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.light.tol$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = NA, lifeStage = 1, resources = 1, tolerance = 1))
               , "`mat.PFG.light.tol$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light.tol$lifeStage : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = 1, resources = 1, tolerance = 1))
               , "`mat.PFG.light.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = NA, resources = 1, tolerance = 1))
               , "`mat.PFG.light.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "", resources = 1, tolerance = 1))
               , "`mat.PFG.light.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light.tol$resources : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = 1, tolerance = 1))
               , "`mat.PFG.light.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = NA, tolerance = 1))
               , "`mat.PFG.light.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "", tolerance = 1))
               , "`mat.PFG.light.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.light.tol$tolerance : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = c(NA, 1)))
               , "`mat.PFG.light.tol$tolerance` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.light.tol$tolerance : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = -1))
               , "`mat.PFG.light.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = 1.5))
               , "`mat.PFG.light.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light.tol$strategy_tol : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = 1))
               , "`mat.PFG.light.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = NA))
               , "`mat.PFG.light.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = factor("a")))
               , "`mat.PFG.light.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
})




## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.soil", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.soil : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = NA)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = NULL)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = "")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = 1)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = factor(1))
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = matrix(1))
               , "`mat.PFG.soil` must be a data.frame")
  
  ## TEST mat.PFG.soil : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame())
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(1))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(1,2,3))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = rep("H", 2), strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = NA, type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = "", type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$type : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = NA, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = 2, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  ## TEST mat.PFG.soil$strategy_contrib : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = 1))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = NA))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = ""))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$strategy_ag : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = 1, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = NA, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = "", strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$active_germ_low : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = c(NA, 1)
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_low : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = -1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1.5
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_medium : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                      , active_germ_medium = c(NA, 1), active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_medium : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = -1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1.5, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_high : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = c(NA, 1)
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_high : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = -1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1.5
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$soil_contrib : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = NA, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = "a", soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = factor(1), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = factor("a"), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_contrib : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = c(NA, 1), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_min : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = NA, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = "a", soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = factor(1), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = factor("a"), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_min : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = c(NA, 1), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = NA))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = "a"))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = factor(1)))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = factor("a")))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = c(NA, 1)))
               , "`mat.PFG.soil$soil_tol_max` must not contain NA values", fixed = TRUE)
  
  
  
  ## TEST mat.PFG.soil$soil_tol_min : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain values equal or inferior to `mat.PFG.soil$soil_contrib`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 0))
               , "`mat.PFG.soil$soil_tol_max` must contain values equal or superior to `mat.PFG.soil$soil_contrib`"
               , fixed = TRUE)
  
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.soil.tol", {
  
  ## TEST mat.PFG.soil.tol : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = NA)
               , "`mat.PFG.soil.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = "")
               , "`mat.PFG.soil.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = 1)
               , "`mat.PFG.soil.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = matrix(1))
               , "`mat.PFG.soil.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.soil.tol : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame())
               , "`mat.PFG.soil.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(1))
               , "`mat.PFG.soil.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(1,2))
               , "Column names of `mat.PFG.soil.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.soil.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil.tol$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = NA, lifeStage = 1
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol$lifeStage : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = 1
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = NA
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = ""
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol$resources : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = NA, tolerance = 1))
               , "`mat.PFG.soil.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "", tolerance = 1))
               , "`mat.PFG.soil.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil.tol$tolerance : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "Low", tolerance = c(NA, 1)))
               , "`mat.PFG.soil.tol$tolerance` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol$tolerance : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "Low", tolerance = -1))
               , "`mat.PFG.soil.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "Low", tolerance = 1.5))
               , "`mat.PFG.soil.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol$strategy_tol : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = 1))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = NA))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                          , mat.PFG.light.tol = data.frame(PFG = 1, strategy_tol = "pioneer")
                                          , mat.PFG.soil = data.frame(PFG = 1
                                                                      , strategy_ag = "indifferent"
                                                                      , strategy_contrib = "ubiquist")
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = factor("a")))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
})


