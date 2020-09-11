library(RFate)
context("PRE_FATE.params_PFGsoil() function")

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with missing data", {
  expect_error(PRE_FATE.params_PFGsoil()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
})

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGsoil(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
})

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : mat.PFG.soil", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.soil : data.frame
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = NA)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = NULL)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = "")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = 1)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = factor(1))
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = matrix(1))
               , "`mat.PFG.soil` must be a data.frame")
  
  ## TEST mat.PFG.soil : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame())
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1,2,3,4,5,6,7,8))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, (active_germ_low), (active_germ_medium), (active_germ_high), (strategy_ag), soil_contrib, soil_tol_min, soil_tol_max, (strategy_contrib))"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil : correct names of columns
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1,2,3))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `(active_germ_low)`, `(active_germ_medium)`, `(active_germ_high)`, `(strategy_ag)`, `soil_contrib`, `soil_tol_min`, `soil_tol_max` and `(strategy_contrib)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$PFG : different values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = rep("H", 2), strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = NA, type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$PFG : length > 0
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = "", type = "H", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$type : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = NA, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = 2, strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "", strategy_contrib = "indifferent"))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = TRUE)
  
  ## TEST mat.PFG.soil$strategy_contrib : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = 1))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = NA))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", strategy_contrib = ""))
               , "`mat.PFG.soil$strategy_contrib` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$strategy_ag : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = 1, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = NA, strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = "", strategy_contrib = "ubiquist"))
               , "`mat.PFG.soil$strategy_ag` must be either `poor_lover`, `indifferent` or `rich_lover`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$active_germ_low : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = c(NA, 1)
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_low : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = -1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1.5
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_low` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_medium : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                   , active_germ_medium = c(NA, 1), active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_medium : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = -1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1.5, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_medium` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_high : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = c(NA, 1)
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$active_germ_high : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = -1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1.5
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$active_germ_high` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.soil$soil_contrib : numeric values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = NA, soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = "a", soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = factor(1), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = factor("a"), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_contrib : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = c(NA, 1), soil_tol_min = 1, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_contrib` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_min : numeric values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = NA, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = "a", soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = factor(1), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = factor("a"), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_min : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = c(NA, 1), soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : numeric values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = NA))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = "a"))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = factor(1)))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = factor("a")))
               , "`mat.PFG.soil$soil_tol_max` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = c(1,2), active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = c(NA, 1)))
               , "`mat.PFG.soil$soil_tol_max` must not contain NA values", fixed = TRUE)
  
  
  
  ## TEST mat.PFG.soil$soil_tol_min : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 1))
               , "`mat.PFG.soil$soil_tol_min` must contain values equal or inferior to `mat.PFG.soil$soil_contrib`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.soil$soil_tol_max : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, active_germ_low = 1
                                                                   , active_germ_medium = 1, active_germ_high = 1
                                                                   , soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 0))
               , "`mat.PFG.soil$soil_tol_max` must contain values equal or superior to `mat.PFG.soil$soil_contrib`"
               , fixed = TRUE)
  
})

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : mat.PFG.tol", {
  
  ## TEST mat.PFG.tol : data.frame
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = NA)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = "")
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = 1)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = matrix(1))
               , "`mat.PFG.tol` must be a data.frame")
  
  
  ## TEST mat.PFG.tol : correct number of rows and columns
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame())
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(1))
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, resources, tolerance, (strategy_tol)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol : correct names of columns
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(1,2))
               , "Column names of `mat.PFG.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.tol` must be `PFG`, `lifeStage`, `resources`, `tolerance` and `(strategy_tol)`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.tol$PFG : length > 0
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = NA, lifeStage = 1
                                                                  , resources = 1, tolerance = 1))
               , "`mat.PFG.tol$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$lifeStage : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = 1
                                                                  , resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = NA
                                                                  , resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = ""
                                                                  , resources = 1, tolerance = 1))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$resources : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = 1, tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = NA, tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = "", tolerance = 1))
               , "`mat.PFG.tol$resources` must be either `Low`, `Medium` or `High`"
               , fixed = TRUE)
  

  ## TEST mat.PFG.tol$tolerance : no NA values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = "Low", tolerance = c(NA, 1)))
               , "`mat.PFG.tol$tolerance` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.tol$tolerance : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = "Low", tolerance = -1))
               , "`mat.PFG.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                  , resources = "Low", tolerance = 1.5))
               , "`mat.PFG.tol$tolerance` must be either `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9` or `10`"
               , fixed = TRUE)
  
  ## TEST mat.PFG.tol$strategy_tol : correct values
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = 1))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = NA))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1
                                                                   , strategy_ag = "indifferent"
                                                                   , strategy_contrib = "ubiquist")
                                       , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = factor("a")))
               , "`mat.PFG.tol$strategy_tol` must be either `full_light`, `pioneer`, `ubiquist`, `semi_shade` or `undergrowth`"
               , fixed = TRUE)
})



## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 1, CONTRIB scenario 1, SOIL_TOL scenario 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H"
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H"
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL
                                         , opt.folder.name = "")
                 , "already exists. It will be replaced.")
  expect_warning(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H"
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL
                                         , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H"
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL
                                         , opt.folder.name = "TEST")
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/TEST/SOIL_1.txt has been successfully created !")
})



## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 2, CONTRIB scenario 1, SOIL_TOL scenario 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1
                                                                     , active_germ_low = 1
                                                                     , active_germ_medium = 5
                                                                     , active_germ_high = 9
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 3, CONTRIB scenario 1, SOIL_TOL scenario 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1
                                                                     , strategy_ag = "poor_lover"
                                                                     , soil_contrib = 2
                                                                     , soil_tol_min = 1
                                                                     , soil_tol_max = 3)
                                         , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
})



## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 3, CONTRIB scenario 2, SOIL_TOL scenario 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1
                                                                     , strategy_ag = "poor_lover"
                                                                     , strategy_contrib = "ubiquist")
                                         , mat.PFG.tol = NULL)
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 3, CONTRIB scenario 2, SOIL_TOL scenario 2", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1
                                                                     , strategy_ag = "poor_lover"
                                                                     , strategy_contrib = "ubiquist")
                                         , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant"
                                                                    , resources = "Low", tolerance = 3))
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
})




## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output : ACTIVE_GERM scenario 3, CONTRIB scenario 2, SOIL_TOL scenario 3", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, strategy_ag = "poor_lover"
                                                                     , strategy_contrib = "ubiquist")
                                         , mat.PFG.tol = data.frame(PFG = 1, strategy_tol = "ubiquist"))
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
})
