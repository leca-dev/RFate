library(RFate)
context("PRE_FATE.params_PFGlight() function")

## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with missing data", {
  expect_error(PRE_FATE.params_PFGlight()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGlight(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : mat.PFG.light", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.light : data.frame
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation")
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = NA)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = NULL)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = "")
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = 1)
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = factor(1))
               , "`mat.PFG.light` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = matrix(1))
               , "`mat.PFG.light` must be a data.frame")
  
  ## TEST mat.PFG.light : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = data.frame())
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = data.frame(1))
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = data.frame(1,2,3,4,5,6,7))
               , "`mat.PFG.light` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), (light_need))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light : correct names of columns
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = data.frame(1,2))
               , "Column names of `mat.PFG.light` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)` and `(light_need)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.light = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.light` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)` and `(light_need)`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.light$PFG : different values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, type = c(2,2)))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = NA, type = 2))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,NA), type = c(2,2)))
               , "`mat.PFG.light$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.light$PFG : length > 0
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = "", type = 1))
               , "`mat.PFG.light$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.PFG.light$type : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, type = NA))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, type = 2))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, type = ""))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), type = c("H",NA)))
               , "`mat.PFG.light$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  ## TEST mat.PFG.light$light_need : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), type = "H", light_need = c(3,NA)))
               , "`mat.PFG.light$light_need` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$light_need : no NA values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, type = "H", light_need = 1.5))
               , "`mat.PFG.light$light_need` must be either `0`, `1`, `2`, `3`, `4` or `5`", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_low : no NA values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = c(3,NA)
                                                                     , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_low : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = -1
                                                                     , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1.5
                                                                     , active_germ_medium = 1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  
  ## TEST mat.PFG.light$active_germ_medium : no NA values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                     , active_germ_medium = c(3,NA), active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_medium : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                     , active_germ_medium = -1, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                     , active_germ_medium = 1.5, active_germ_high = 1))
               , "`mat.PFG.light$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  
  ## TEST mat.PFG.light$active_germ_high : no NA values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                     , active_germ_medium = 1, active_germ_high = c(3,NA)))
               , "`mat.PFG.light$active_germ_high` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.light$active_germ_high : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                     , active_germ_medium = 1, active_germ_high = -1))
               , "`mat.PFG.light$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                     , active_germ_medium = 1, active_germ_high = 1.5))
               , "`mat.PFG.light$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)  
  
  ## TEST mat.PFG.light$strategy_ag : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = NA))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = 2))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = ""))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = c(1,2), strategy_ag = c("indifferent",NA)))
               , "`mat.PFG.light$strategy_ag` must be either `light_lover`, `indifferent` or `shade_lover`", fixed = TRUE)
  
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : mat.PFG.tol", {
  
  ## TEST mat.PFG.tol : data.frame
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = NA)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = "")
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = 1)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = matrix(1))
               , "`mat.PFG.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.tol : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame())
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(1))
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol : correct names of columns
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(1,2))
               , "Column names of `mat.PFG.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$PFG : length > 0
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = NA, lifeStage = 1, resources = 1, tolerance = 1))
               , "`mat.PFG.tol$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$lifeStage : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = 1, resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = NA, resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "", resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$resources : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = 1, tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = NA, tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "", tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$tolerance : no NA values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = c(NA, 1)))
               , "`mat.PFG.tol$tolerance` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$tolerance : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = -1))
               , "`mat.PFG.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = 1.5))
               , "`mat.PFG.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$strategy_tol : correct values
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = 1))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = NA))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "indifferent")
                                        , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = factor("a")))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 1, LIGHT_TOL scenario 0", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL)
                 , "Missing data! The `LIGHT` parameter has not been set. Please check.")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL)
                 , "Missing data! The `LIGHT_TOL` parameter has not been set. Please check.")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL
                                          , opt.folder.name = "")
                 , "already exists. It will be replaced.")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL
                                          , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, type = "H")
                                          , mat.PFG.tol = NULL
                                          , opt.folder.name = "TEST")
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/TEST/LIGHT_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 2, LIGHT_TOL scenario 0", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, active_germ_low = 1
                                                                       , active_germ_medium = 5, active_germ_high = 9)
                                          , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 3, LIGHT_TOL scenario 0", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "light_lover")
                                          , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 3, LIGHT_TOL scenario 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "light_lover"
                                                                       , type = "H", light_need = 4)
                                          , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 3, LIGHT_TOL scenario 2", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "light_lover")
                                          , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = "pioneer"))
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
})



## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output : ACTIVE_GERM scenario 3, LIGHT_TOL scenario 3", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.light = data.frame(PFG = 1, strategy_ag = "light_lover")
                                          , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", resources = "Low", tolerance = 3))
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_1.txt has been successfully created !")
})
