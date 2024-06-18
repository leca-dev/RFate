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
  # expect_error(SAVE_FATE.step2_parameters(name.dataset = "A", name.simulation = matrix(seq(2), ncol=2))
  #              , "`name.simulation` does not exist or does not contain a DATA/ folder")
  
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
               , "`mat.PFG.succ$immature_size` must be an integer > 0"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = 1.5))
               , "`mat.PFG.succ$immature_size` must be an integer > 0"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                      , height = 3, maturity = 4
                                                                      , longevity = 10, immature_size = 101))
               , "`mat.PFG.succ$immature_size` must contain values between `0` and `100`"
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
                                          , mat.PFG.soil = "")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = 1)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = factor(1))
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = matrix(1))
               , "`mat.PFG.soil` must be a data.frame")
  
  ## TEST mat.PFG.soil : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame())
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(1))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(1,2,3))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = rep("H", 2), strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = NA, type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = "", type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$type : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = NA, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = 2, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  ## TEST mat.PFG.soil$strategy_contrib : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = 1))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = NA))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = ""))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$strategy_ag : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = 1, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = NA, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = "", strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$active_germ_low : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = NA, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = "a", soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = factor(1), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = NA, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = "a", soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = factor(1), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = NA))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = "a"))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                      , active_germ_medium = 1, active_germ_high = 1
                                                                      , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = factor(1)))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil.tol = "")
               , "`mat.PFG.soil.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = 1)
               , "`mat.PFG.soil.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = matrix(1))
               , "`mat.PFG.soil.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.soil.tol : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame())
               , "`mat.PFG.soil.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(1))
               , "`mat.PFG.soil.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(1,2))
               , "Column names of `mat.PFG.soil.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.soil.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil.tol$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = 1
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = NA
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = 1, tolerance = 1))
               , "`mat.PFG.soil.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = NA, tolerance = 1))
               , "`mat.PFG.soil.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "Low", tolerance = c(NA, 1)))
               , "`mat.PFG.soil.tol$tolerance` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil.tol$tolerance : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                          , resources = "Low", tolerance = -1))
               , "`mat.PFG.soil.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
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
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = 1))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = NA))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.soil.tol = data.frame(PFG = 1, strategy_tol = factor("a")))
               , "`mat.PFG.soil.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
})



## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.disp", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.disp : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = "")
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = 1)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = factor(1))
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = matrix(1))
               , "`mat.PFG.disp` must be a data.frame")
  
  ## TEST mat.PFG.disp :correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame())
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(1))
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.disp : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(1, 2, 3, 4))
               , "Column names of `mat.PFG.disp` must be `PFG`, `d50`, `d99` and `ldd`")
  
  ## TEST mat.PFG.disp$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = c(2,2), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = "", d50 = NA, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.disp$d50 : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = NA, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = "a", d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = factor(1), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = factor("a"), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$d50 : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = c(2,NA), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.disp$d99 : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = NA, ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = "a", ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor(1), ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor("a"), ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$d99 : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = c(3,NA), ldd = 4))
               , "`mat.PFG.disp$d99` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$ldd : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = NA))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = "a"))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor(1)))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor("a")))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$ldd : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = 3, ldd = c(4,NA)))
               , "`mat.PFG.disp$ldd` must not contain NA values", fixed = TRUE)
})



## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.dist", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.dist : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = "")
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = 1)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = factor(1))
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = matrix(1))
               , "`mat.PFG.dist` must be a data.frame")
  
  ## TEST mat.PFG.dist : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame())
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(1))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(1,2,3,4))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (PFG, type, maturity, longevity, age_above_150cm)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.dist` must be `PFG`, `type`, `maturity`, `longevity` and `age_above_150cm`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.dist$PFG : different values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = c(2,2), maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = NA, type = 2, maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,NA), type = c(2,2), maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = "", type = 1, maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.dist$type : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = NA, maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = 2, maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "", maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,2), type = c("H",NA), maturity = 1
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  
  
  ## TEST mat.PFG.dist$maturity : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = NA
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = "a"
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = factor(1)
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$maturity : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = c(3,NA)
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$maturity : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 2
                                                                      , longevity = 1, age_above_150cm = 1))
               , "`mat.PFG.dist$maturity` must contain values equal or inferior to `mat.PFG.dist$longevity`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist$longevity : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = NA, age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = "a", age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = factor(1), age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$longevity : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 1
                                                                      , longevity = c(3,NA), age_above_150cm = 1))
               , "`mat.PFG.dist$longevity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$age_above_150cm : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = 1, age_above_150cm = NA))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = 1, age_above_150cm = "a"))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = 1, type = "H", maturity = 1
                                                                      , longevity = 1, age_above_150cm = factor(1)))
               , "`mat.PFG.dist$age_above_150cm` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist$age_above_150cm : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = c(1,2), type = "H", maturity = 1
                                                                      , longevity = 1, age_above_150cm = c(3,NA)))
               , "`mat.PFG.dist$age_above_150cm` must not contain NA values", fixed = TRUE)
  
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.dist.tol", {
  
  ## TEST mat.PFG.dist.tol : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = "")
               , "`mat.PFG.dist.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = 1)
               , "`mat.PFG.dist.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = matrix(1))
               , "`mat.PFG.dist.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.dist.tol : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame())
               , "`mat.PFG.dist.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(1))
               , "`mat.PFG.dist.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.dist.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(1,2,3))
               , "Column names of `mat.PFG.dist.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.dist.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$nameDist : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = "", PFG = 1, strategy_tol = 1))
               , "`mat.PFG.dist.tol$nameDist` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "", strategy_tol = 1))
               , "`mat.PFG.dist.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = NA, strategy_tol = 1))
               , "`mat.PFG.dist.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$PFG : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = "B", type = "H", maturity = 1
                                                                      , longevity = 1 , age_above_150cm = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = 1))
               , "`mat.PFG.dist.tol$PFG` must be either `H`, `C`, `P` or `B`", fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$breakAge : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = NA, resproutAge = 1
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = "a", resproutAge = 1
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = factor(1), resproutAge = 1
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$breakAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$breakAge : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = c(1,NA), resproutAge = 1
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$breakAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$resproutAge : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = 1, resproutAge = NA
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = 1, resproutAge = "a"
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = 1, resproutAge = factor(1)
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$resproutAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$resproutAge : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , breakAge = 1, resproutAge = c(1,NA)
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$resproutAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$responseStage : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = c(1,NA)
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$responseStage` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$responseStage : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1.5
                                                                          , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$responseStage` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$killedIndiv : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = c(1,NA), resproutIndiv = 1))
               , "`mat.PFG.dist.tol$killedIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$killedIndiv : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = -1, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = 1.5, resproutIndiv = 1))
               , "`mat.PFG.dist.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$resproutIndiv : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = 1, resproutIndiv = c(1,NA)))
               , "`mat.PFG.dist.tol$resproutIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.dist.tol$resproutIndiv : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = 1, resproutIndiv = -1))
               , "`mat.PFG.dist.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", responseStage = 1
                                                                          , killedIndiv = 1, resproutIndiv = 1.5))
               , "`mat.PFG.dist.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.dist.tol$strategy_tol : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = 1))
               , "`mat.PFG.dist.tol$strategy_tol` must be either `indifferent`, `mowing_herbs`, `mowing_trees`, `grazing_herbs_1`, `grazing_herbs_2`, `grazing_herbs_3`, `grazing_trees_1`, `grazing_trees_2` or `grazing_trees_3`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist.tol = data.frame(nameDist = 1, PFG = "A", strategy_tol = NA))
               , "`mat.PFG.dist.tol$strategy_tol` must be either `indifferent`, `mowing_herbs`, `mowing_trees`, `grazing_herbs_1`, `grazing_herbs_2`, `grazing_herbs_3`, `grazing_trees_1`, `grazing_trees_2` or `grazing_trees_3`"
               , fixed = TRUE)
  
})



## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.drought", {
  
  ## TEST mat.PFG.drought : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = "")
               , "`mat.PFG.drought` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = 1)
               , "`mat.PFG.drought` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = matrix(1))
               , "`mat.PFG.drought` must be a data.frame")
  
  
  ## TEST mat.PFG.drought : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame())
               , "`mat.PFG.drought` does not have the appropriate number of rows (>0) or columns (PFG, threshold_moderate, threshold_severe, counter_recovery, counter_sens, counter_cum, (strategy_drou))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(1))
               , "`mat.PFG.drought` does not have the appropriate number of rows (>0) or columns (PFG, threshold_moderate, threshold_severe, counter_recovery, counter_sens, counter_cum, (strategy_drou))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.drought` does not have the appropriate number of rows (>0) or columns (PFG, threshold_moderate, threshold_severe, counter_recovery, counter_sens, counter_cum, (strategy_drou))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.drought : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.drought` must be `PFG`, `threshold_moderate`, `threshold_severe`, `counter_recovery`, `counter_sens`, `counter_cum` and `(strategy_drou)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(1,2,3,4,5,6))
               , "Column names of `mat.PFG.drought` must be `PFG`, `threshold_moderate`, `threshold_severe`, `counter_recovery`, `counter_sens`, `counter_cum` and `(strategy_drou)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.drought$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = ""
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$PFG` must contain a character value of length > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = NA
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  
  
  ## TEST mat.PFG.drought$threshold_moderate : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = NA
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_moderate` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = "a"
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_moderate` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = factor(1)
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_moderate` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$threshold_moderate : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = c(1,NA)
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_moderate` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$threshold_severe : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = NA
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_severe` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = "a"
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_severe` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = factor(1)
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_severe` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$threshold_severe : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = c(1,NA)
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_severe` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$threshold_severe : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 2
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$threshold_severe` must contain values equal or inferior to `mat.PFG.drought$threshold_moderate`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_recovery : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = NA
                                                                         , counter_sens = 1
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_recovery` must not contain NA values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = c(1,NA)
                                                                         , counter_sens = 1
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_recovery` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_recovery : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = "a"
                                                                         , counter_sens = 1
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_recovery` must be an integer > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = factor(1)
                                                                         , counter_sens = 1
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_recovery` must be an integer > 0", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_sens : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = NA
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_sens` must not contain NA values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = c(1,NA)
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_sens` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_sens : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = "a"
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_sens` must be an integer > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = factor(1)
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_sens` must be an integer > 0", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_cum : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = 1
                                                                         , counter_cum = NA))
               , "`mat.PFG.drought$counter_cum` must not contain NA values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = 1
                                                                         , counter_cum = c(1,NA)))
               , "`mat.PFG.drought$counter_cum` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_cum : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = 1
                                                                         , counter_cum = "a"))
               , "`mat.PFG.drought$counter_cum` must be an integer > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = 1
                                                                         , counter_cum = factor(1)))
               , "`mat.PFG.drought$counter_cum` must be an integer > 0", fixed = TRUE)
  
  ## TEST mat.PFG.drought$counter_sens : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , counter_recovery = 1
                                                                         , counter_sens = 2
                                                                         , counter_cum = 1))
               , "`mat.PFG.drought$counter_sens` must contain values equal or inferior to `mat.PFG.drought$counter_cum`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.drought$strategy_drou : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = 1))
               , "`mat.PFG.drought$strategy_drou` must be either `herbs`, `chamaephytes` or `trees_shrubs`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought = data.frame(PFG = "A"
                                                                         , threshold_moderate = 1
                                                                         , threshold_severe = 1
                                                                         , strategy_drou = NA))
               , "`mat.PFG.drought$strategy_drou` must be either `herbs`, `chamaephytes` or `trees_shrubs`"
               , fixed = TRUE)
  
})

## INPUTS
test_that("SAVE_FATE.step2_parameters gives error with wrong data : mat.PFG.drought.tol", {
  
  ## TEST mat.PFG.drought.tol : data.frame
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = "")
               , "`mat.PFG.drought.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = 1)
               , "`mat.PFG.drought.tol` must be a data.frame")
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = matrix(1))
               , "`mat.PFG.drought.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.drought.tol : correct number of rows and columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame())
               , "`mat.PFG.drought.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(1))
               , "`mat.PFG.drought.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.drought.tol` does not have the appropriate number of rows (>0) or columns (nameDist, PFG, responseStage, (breakAge), (resproutAge), killedIndiv, resproutIndiv, (strategy_tol))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol : correct names of columns
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(1,2,3))
               , "Column names of `mat.PFG.drought.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.drought.tol` must be `nameDist`, `PFG`, `responseStage`, `(breakAge)`, `(resproutAge)`, `killedIndiv`, `resproutIndiv` and `(strategy_tol)`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$nameDist : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "", PFG = 1, strategy_tol = 1))
               , "`mat.PFG.drought.tol$nameDist` must be either `immediate` or `delayed`", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "first", PFG = 1, strategy_tol = 1))
               , "`mat.PFG.drought.tol$nameDist` must be either `immediate` or `delayed`", fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$PFG : length > 0
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "", strategy_tol = 1))
               , "`mat.PFG.drought.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = NA, strategy_tol = 1))
               , "`mat.PFG.drought.tol$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$PFG : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.dist = data.frame(PFG = "B", type = "H", maturity = 1
                                                                      , longevity = 1 , age_above_150cm = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", strategy_tol = 1))
               , "`mat.PFG.drought.tol$PFG` must be either `H`, `C`, `P` or `B`", fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$breakAge : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = NA, resproutAge = 1
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = "a", resproutAge = 1
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$breakAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = factor(1), resproutAge = 1
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$breakAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$breakAge : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          ,  mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                              , breakAge = c(1,NA), resproutAge = 1
                                                                              , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$breakAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$resproutAge : numeric values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = 1, resproutAge = NA
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = 1, resproutAge = "a"
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$resproutAge` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = 1, resproutAge = factor(1)
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$resproutAge` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$resproutAge : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , breakAge = 1, resproutAge = c(1,NA)
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$resproutAge` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$responseStage : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = c(1,NA)
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$responseStage` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$responseStage : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A"
                                                                             , responseStage = 1.5
                                                                             , killedIndiv = 1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$responseStage` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$killedIndiv : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = c(1,NA), resproutIndiv = 1))
               , "`mat.PFG.drought.tol$killedIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$killedIndiv : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = -1, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = 1.5, resproutIndiv = 1))
               , "`mat.PFG.drought.tol$killedIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$resproutIndiv : no NA values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = 1, resproutIndiv = c(1,NA)))
               , "`mat.PFG.drought.tol$resproutIndiv` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.drought.tol$resproutIndiv : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = 1, resproutIndiv = -1))
               , "`mat.PFG.drought.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", responseStage = 1
                                                                             , killedIndiv = 1, resproutIndiv = 1.5))
               , "`mat.PFG.drought.tol$resproutIndiv` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.drought.tol$strategy_tol : correct values
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", strategy_tol = 1))
               , "`mat.PFG.drought.tol$strategy_tol` must be either `herbs_cham_1`, `herbs_cham_2`, `herbs_cham_3`, `trees_1`, `trees_2` or `trees_3`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step2_parameters(name.dataset = "A"
                                          , name.simulation = "FATE_simulation"
                                          , strata.limits = 1
                                          , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 1
                                                                      , maturity = 1, longevity = 1)
                                          , mat.PFG.drought.tol = data.frame(nameDist = "immediate", PFG = "A", strategy_tol = NA))
               , "`mat.PFG.drought.tol$strategy_tol` must be either `herbs_cham_1`, `herbs_cham_2`, `herbs_cham_3`, `trees_1`, `trees_2` or `trees_3`"
               , fixed = TRUE)
  
})
