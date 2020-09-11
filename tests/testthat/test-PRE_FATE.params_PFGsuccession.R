library(RFate)
context("PRE_FATE.params_PFGsuccession() function")

## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with missing data", {
  expect_error(PRE_FATE.params_PFGsuccession()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGsuccession(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with wrong data : mat.PFG.succ", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.succ : data.frame
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = "")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be a data.frame")
  
  ## TEST mat.PFG.succ : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, (max_abundance), (potential_fecundity), (immature_size), (is_alien), (flammability))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, (max_abundance), (potential_fecundity), (immature_size), (is_alien), (flammability))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.succ : correct names of columns
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1,2,3,4,5,6))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1,2,3,4,5,6,7))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1,2,3,4,5,6,7,8))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(1,2,3,4,5,6,7,8,9))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, hop = 1))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity`, `(max_abundance)`, `(potential_fecundity)`, `(immature_size)`, `(is_alien)` and `(flammability)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$PFG : different values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = c(2,2)
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = NA, type = 2
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,NA), type = c(2,2)
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$PFG : length > 0
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = "", type = NA
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$type : correct values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = NA
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = 2
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = ""
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = c(2,NA)
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$height : numeric values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = NA, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = "a", maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = factor(1), maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$height` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$height : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = c(3,NA), maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$height` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$maturity : numeric values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = NA
                                                                         , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = "a"
                                                                         , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = factor(1)
                                                                         , longevity = 10))
               , "`mat.PFG.succ$maturity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$maturity : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = c(4,NA)
                                                                         , longevity = 10))
               , "`mat.PFG.succ$maturity` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$longevity : numeric values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = NA))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = "a"))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = factor(1)))
               , "`mat.PFG.succ$longevity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$longevity : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = c(10,NA)))
               , "`mat.PFG.succ$longevity` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$maturity : correct values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 2))
               , "`mat.PFG.succ$maturity` must contain values equal or inferior to `mat.PFG.succ$longevity`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$max_abundance : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, max_abundance = c(10,NA)))
               , "`mat.PFG.succ$max_abundance` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$max_abundance : correct values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, max_abundance = "a"))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, max_abundance = 1.5))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, max_abundance = 10))
               , "`mat.PFG.succ$max_abundance` must be either `1`, `2` or `3`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$potential_fecundity : numeric values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, potential_fecundity = NA))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, potential_fecundity = "a"))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, potential_fecundity = factor(1)))
               , "`mat.PFG.succ$potential_fecundity` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$potential_fecundity : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, potential_fecundity = c(10,NA)))
               , "`mat.PFG.succ$potential_fecundity` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$immature_size : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, immature_size = c(10,NA)))
               , "`mat.PFG.succ$immature_size` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$immature_size : correct values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, immature_size = "a"))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, immature_size = 1.5))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, immature_size = 11))
               , "`mat.PFG.succ$immature_size` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  
  ## TEST mat.PFG.succ$is_alien : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, is_alien = c(10,NA)))
               , "`mat.PFG.succ$is_alien` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$is_alien : correct values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, is_alien = "a"))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, is_alien = 1.5))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, is_alien = 11))
               , "`mat.PFG.succ$is_alien` must be either `0` or `1`", fixed = TRUE)
  
  
  ## TEST mat.PFG.succ$flammability : numeric values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, flammability = NA))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, flammability = "a"))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, flammability = factor(1)))
               , "`mat.PFG.succ$flammability` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.succ$flammability : no NA values
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H"
                                                                         , height = 3, maturity = 4
                                                                         , longevity = 10, flammability = c(10,NA)))
               , "`mat.PFG.succ$flammability` must not contain NA values", fixed = TRUE)
})


## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with wrong data : strata.limits", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST strata.limits : integer
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                         , type = c("C", "C", "H", "H", "P", "P")
                                                                         , height = c(10, 250, 36, 68, 1250, 550)
                                                                         , maturity = c(5, 5, 3, 3, 8, 9)
                                                                         , longevity = c(12, 200, 25, 4, 110, 70))
                                             , strata.limits = NA)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                         , type = c("C", "C", "H", "H", "P", "P")
                                                                         , height = c(10, 250, 36, 68, 1250, 550)
                                                                         , maturity = c(5, 5, 3, 3, 8, 9)
                                                                         , longevity = c(12, 200, 25, 4, 110, 70))
                                             , strata.limits = "a")
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                         , type = c("C", "C", "H", "H", "P", "P")
                                                                         , height = c(10, 250, 36, 68, 1250, 550)
                                                                         , maturity = c(5, 5, 3, 3, 8, 9)
                                                                         , longevity = c(12, 200, 25, 4, 110, 70))
                                             , strata.limits = factor(1))
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                         , type = c("C", "C", "H", "H", "P", "P")
                                                                         , height = c(10, 250, 36, 68, 1250, 550)
                                                                         , maturity = c(5, 5, 3, 3, 8, 9)
                                                                         , longevity = c(12, 200, 25, 4, 110, 70))
                                             , strata.limits = -1)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
})



## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                            , type = c("C", "C", "H", "H", "P", "P")
                            , height = c(10, 250, 36, 68, 1250, 550)
                            , maturity = c(5, 5, 3, 3, 8, 9)
                            , longevity = c(12, 200, 25, 4, 110, 70))
  
  ## TEST classic input
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = mat.PFG.succ)
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/SUCC_PFG1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = mat.PFG.succ
                                               , opt.folder.name = "")
                 , "already exists. It will be replaced.")
  
  ## TEST opt.folder.name
  expect_warning(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = mat.PFG.succ
                                               , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = mat.PFG.succ
                                               , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = mat.PFG.succ
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/scen1/SUCC_PFG1.txt has been successfully created !")
  
  ## TEST selection of strata limits
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = "PFG1"
                                                                           , type = "C"
                                                                           , height = 10
                                                                           , maturity = 5
                                                                           , longevity = 12)
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/scen1/SUCC_PFG1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario max_abundance", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)
                                                                           , max_abundance = c(2, 2, 1, 1, 2, 3))
                                               , opt.folder.name = "max_abund")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/max_abund/SUCC_PFG1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario potential_fecundity", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)
                                                                           , potential_fecundity = rep(150, 6))
                                               , opt.folder.name = "pot_fecund")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/pot_fecund/SUCC_PFG1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario immature_size", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)
                                                                           , immature_size = c(8, 8, 10, 10, 5, 6))
                                               , opt.folder.name = "imm_size")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/imm_size/SUCC_PFG1.txt has been successfully created !")
})

## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario is_alien", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)
                                                                           , is_alien = c(0, 0, 1, 0, 0, 0))
                                               , opt.folder.name = "is_alien")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/is_alien/SUCC_PFG1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario flammability", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)
                                                                           , flammability = c(0, 0, 1, 0, 0, 0))
                                               , opt.folder.name = "flammability")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/flammability/SUCC_PFG1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output : scenario no strata.limits reduction", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70))
                                               , strata.limits_reduce = FALSE
                                               , opt.folder.name = "strat")
                 , "The parameter file FATE_simulation/DATA/PFGS/SUCC/strat/SUCC_PFG1.txt has been successfully created !")
})

