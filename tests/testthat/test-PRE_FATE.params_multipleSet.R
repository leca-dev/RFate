library(RFate)
context("PRE_FATE.params_multipleSet() function")

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with missing data", {
  expect_error(PRE_FATE.params_multipleSet()
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(NA)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(NULL)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : name.simulation.1", {
  expect_error(PRE_FATE.params_multipleSet(1)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet("a")
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(factor(1))
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(matrix(seq(2), ncol=2))
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation/DATA")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : file.simulParam.1", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = 1)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = factor("a"))
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "a")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/a` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : file.simulParam.2", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = NA)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = 1)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = factor("a"))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "a")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/a` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt")
               , "You must select different simulation parameter files !")
  file.create("FATE_simulation/PARAM_SIMUL/toto2.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto2.txt")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : name.simulation.2", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , name.simulation.2 = "a")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files.")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "a")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "a")
               , "`name.simulation.2` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = factor(1))
               , "You must select different simulation parameter files !")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = matrix(seq(2), ncol=2))
               , "You must select different simulation parameter files !")
  
  
  if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
  dir.create("FATE_simulation2")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation2/PARAM_SIMUL")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation2/DATA")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation2/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "Wrong name file given!\n `FATE_simulation2/PARAM_SIMUL/toto.txt` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation")
               , "You must select different simulation parameter files !")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto2.txt"
                                           , name.simulation.2 = "FATE_simulation")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  file.create("FATE_simulation2/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : no_simulations", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = NA)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = NULL)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = factor(1))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = "")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = factor("a"))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_maxAbund", {
  
  ## TEST opt.percent_maxAbund : numeric values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_maxAbund = NA)
               , "`opt.percent_maxAbund` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_maxAbund = NULL)
               , "`opt.percent_maxAbund` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_maxAbund = factor(1))
               , "`opt.percent_maxAbund` must contain numeric values")
  
  ## TEST opt.percent_maxAbund : correct values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_maxAbund = -2)
               , "`opt.percent_maxAbund` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_maxAbund = 1.01)
               , "`opt.percent_maxAbund` must contain values between `0` and `1`")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_seeding", {
  
  ## TEST opt.percent_seeding : numeric values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = NA)
               , "`opt.percent_seeding` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = NULL)
               , "`opt.percent_seeding` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = factor(1))
               , "`opt.percent_seeding` must contain numeric values")
  
  ## TEST opt.percent_seeding : correct values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = -2)
               , "`opt.percent_seeding` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = 1.01)
               , "`opt.percent_seeding` must contain values between `0` and `1`")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_light", {
  
  ## TEST opt.percent_light : numeric values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = NA)
               , "`opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = NULL)
               , "`opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = factor(1))
               , "`opt.percent_light` must contain numeric values")
  
  ## TEST opt.percent_light : correct values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = -2)
               , "`opt.percent_light` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = 1.01)
               , "`opt.percent_light` must contain values between `0` and `1`")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_soil", {
  
  ## TEST opt.percent_soil : numeric values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_soil = NA)
               , "`opt.percent_soil` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_soil = NULL)
               , "`opt.percent_soil` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_soil = factor(1))
               , "`opt.percent_soil` must contain numeric values")
  
  ## TEST opt.percent_soil : correct values
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_soil = -2)
               , "`opt.percent_soil` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_soil = 1.01)
               , "`opt.percent_soil` must contain values between `0` and `1`")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : do...", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "You must select some parameters to vary !")
})



## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : within file.simulParam.1", {
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  
  ## TEST file.simulParam.1 with no_simulations
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "The file FATE_simulation/PARAM_SIMUL/toto.txt is empty. Please check.")
  cat("Yo\n", file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "The file FATE_simulation/PARAM_SIMUL/toto.txt does not contain any parameter values with the --PARAM-- flag. Please check.")
  cat("--Yo--\n", file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "The file FATE_simulation/PARAM_SIMUL/toto.txt does not contain the --END_OF_FILE-- flag. Please check.")
  cat("--Yo--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/toto.txt)"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/glob.txt\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "Wrong name file given!\n `./FATE_simulation/glob.txt` does not exist"
               , fixed = TRUE)
  file.create("FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "The file ./FATE_simulation/glob.txt is empty. Please check.")
  cat("Yo", file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10)
               , "The flag --PFG_PARAMS_LIFE_HISTORY-- in the file FATE_simulation/PARAM_SIMUL/toto.txt does not contain any value. Please check."
               , fixed = TRUE)
  
  
  ## TEST file.simulParam.1 and globalParam file
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat("MAX_ABUND_LOW 10000", file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.no_strata = FALSE
                                           , do.DISPERSAL.mode = FALSE)
               , "The file ./FATE_simulation/glob.txt does not contain any of the required parameter values (NO_PFG, SIMULATION_DURATION, ...). Please check."
               , fixed = TRUE)
  cat("MAX_ABUND_LOW 10000\nTRUC 3", file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (DO_DISPERSAL) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat("MAX_ABUND_LOW 10000\nTRUC 3\nDO_DISPERSAL 0"
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (DO_HAB_SUITABILITY) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat("MAX_ABUND_LOW 10000\nTRUC 3\nDO_DISPERSAL 0\nDO_HAB_SUITABILITY 0"
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (DO_LIGHT_COMPETITION) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat("MAX_ABUND_LOW 10000\nTRUC 3\nDO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0"
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (DO_SOIL_COMPETITION) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nTRUC 3\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (DO_DISTURBANCES) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nTRUC 3\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (NO_PFG) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (NO_STRATA) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\nNO_STRATA 4\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (SIMULATION_DURATION) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (SEEDING_DURATION) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (SEEDING_TIMESTEP) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 2\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (SEEDING_INPUT) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("MAX_ABUND_LOW 10000\nNO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 2\nSEEDING_INPUT 100\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (MAX_ABUND_MEDIUM) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 2\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `flag` (MAX_ABUND_HIGH) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  cat(paste0("NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 2\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 0\nDO_HAB_SUITABILITY 0\n"
             , "DO_LIGHT_COMPETITION 0\nDO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0")
      , file = "FATE_simulation/glob.txt")
  
  ## TEST file.simulParam.1 and mask
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "The flag --MASK-- in the file FATE_simulation/PARAM_SIMUL/toto.txt does not contain any value. Please check."
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/glob.txt\n--MASK--\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong type of data!\n `name.MASK` must contain a character value of length > 0"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/glob.txt\n--MASK--\nmask.txt\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong name file given!\n `FATE_simulation_MULTIPLE_SET/DATA/MASK/mask.txt` does not exist")
  dir.create("FATE_simulation/DATA/MASK")
  file.create("FATE_simulation/DATA/MASK/mask.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "Wrong name file given!\n `FATE_simulation_MULTIPLE_SET/DATA/MASK/mask.txt` does not exist")
  cat("--GLOBAL_PARAMS--\nFATE_simulation/glob.txt\n--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  
  ## TEST file.simulParam.1 and SUCC files
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.DISPERSAL.mode = FALSE
                                           , do.no_strata = FALSE)
               , "There is not the same number of files (`.txt` file starting with `SUCC`) into the FATE_simulation_MULTIPLE_SET/DATA/PFGS/SUCC/ folder as the number of PFG indicated into the file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt"
               , fixed = TRUE)
  dir.create("FATE_simulation/DATA/PFGS")
  dir.create("FATE_simulation/DATA/PFGS/SUCC")
  file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
  file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
  file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
  cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/glob.txt\n"
             , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
             , "--PFG_PARAMS_LIFE_HISTORY--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
             , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
             , "--END_OF_FILE--\n")
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
})





## OUTPUTS
test_that("PRE_FATE.params_multipleSet gives correct output", {
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10.5
                                             , do.DISPERSAL.mode = FALSE
                                             , do.no_strata = FALSE)
                 , "`no_simulations` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10.5
                                             , do.DISPERSAL.mode = FALSE
                                             , do.no_strata = FALSE)
                 , "The parameter 'light_thresh_medium' is not defined in the global file"
                 , fixed = TRUE)
  
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.DISPERSAL.mode = FALSE
                                             , do.no_strata = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.DISPERSAL.mode = FALSE
                                             , do.no_strata = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.params_multipleSet gives correct output with other conditions / scenarios", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
    if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
    
    PRE_FATE.skeletonDirectory()
    
    file.create("FATE_simulation/DATA/MASK/mask.txt")
    
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_1.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_2.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_3.txt")
    
    file.create("FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    cat(paste0("## Test file\n"
               , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
               , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
               , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
               , "DO_DISPERSAL 1\nDISPERSAL_MODE 1\n"
               , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
               , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
        , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    
    file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
    cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
               , "--TEST--\n"
               , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
               , "--PFG_PARAMS_LIFE_HISTORY--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
               , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
               , "--PFG_DISP_PARAMS--\nFATE_simulation/DATA/PFGS/DISP/DISP_1.txt\n"
               , "FATE_simulation/DATA/PFGS/DISP/DISP_2.txt\nFATE_simulation/DATA/PFGS/DISP/DISP_3.txt\n"
               , "--END_OF_FILE--\n")
        , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  }
  
  ## TEST all but do.DISPERSAL.mode and do.no_strata (and light / soil / habsuit not activated)
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.DISPERSAL.mode = FALSE
                                             , do.no_strata = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
  
  ## TEST do.max_abund_low
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.max_abund_low = TRUE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = FALSE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = FALSE
                                             , do.SOIL.retention = FALSE
                                             , do.DISPERSAL.mode = FALSE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
  
  ## TEST do.DISPERSAL.mode
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.max_abund_low = FALSE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = FALSE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = FALSE
                                             , do.SOIL.retention = FALSE
                                             , do.DISPERSAL.mode = TRUE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
  
  {
    if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
    PRE_FATE.skeletonDirectory(name.simulation = "FATE_simulation2")
    file.copy(from = list.files(path = "FATE_simulation"
                                , all.files = TRUE
                                , full.names = TRUE
                                , recursive = TRUE
                                , include.dirs = FALSE)
              , to = sub("FATE_simulation/"
                         , "FATE_simulation2/"
                         , list.files(path = "FATE_simulation"
                                      , all.files = TRUE
                                      , full.names = TRUE
                                      , recursive = TRUE
                                      , include.dirs = FALSE))
              , recursive = TRUE)
    fi = readLines("FATE_simulation2/PARAM_SIMUL/toto.txt")
    fi = sub("FATE_simulation/", "FATE_simulation2/", fi)
    cat(fi, file = "FATE_simulation2/PARAM_SIMUL/toto.txt", sep = "\n")
    
    cat(paste0("## Test file\n"
               , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
               , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
               , "MAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
               , "DO_DISPERSAL 1\nDISPERSAL_MODE 1\n"
               , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
               , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
        , file = "FATE_simulation2/DATA/GLOBAL_PARAMETERS/glob.txt")
  }
  
  ## TEST do.max_abund_low ==> 2 folders
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2"
                                           , file.simulParam.2 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = TRUE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The files do not contain the same parameters to be evaluated."
               , fixed = TRUE)
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 10\nMAX_ABUND_MEDIUM 60000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 1\nDISPERSAL_MODE 1\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation2/DATA/GLOBAL_PARAMETERS/glob.txt")
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2"
                                           , file.simulParam.2 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = TRUE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The global files have different fixed parameter values."
               , fixed = TRUE)
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 1\nDISPERSAL_MODE 1\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation2/DATA/GLOBAL_PARAMETERS/glob.txt")
  fi = readLines("FATE_simulation2/PARAM_SIMUL/toto.txt")
  fi = sub("FATE_simulation2/DATA/MASK/mask.txt", "FATE_simulation2/DATA/MASK/maski.txt", fi)
  cat(fi, file = "FATE_simulation2/PARAM_SIMUL/toto.txt", sep = "\n")
  file.create("FATE_simulation2/DATA/MASK/maski.txt")
  
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2"
                                           , file.simulParam.2 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = TRUE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The simulation files have different fixed parameter values."
               , fixed = TRUE)
  
  fi = readLines("FATE_simulation2/PARAM_SIMUL/toto.txt")
  fi = sub("FATE_simulation2/DATA/MASK/maski.txt", "FATE_simulation2/DATA/MASK/mask.txt", fi)
  cat(fi, file = "FATE_simulation2/PARAM_SIMUL/toto.txt", sep = "\n")
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , name.simulation.2 = "FATE_simulation2"
                                             , file.simulParam.2 = "toto.txt"
                                             , no_simulations = 10
                                             , do.max_abund_low = TRUE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = FALSE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = FALSE
                                             , do.SOIL.retention = FALSE
                                             , do.DISPERSAL.mode = FALSE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !"
                 , fixed = TRUE)
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2"
                                           , file.simulParam.2 = "toto.txt"
                                           , no_simulations = 5
                                           , do.max_abund_low = TRUE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = TRUE
                                           , do.HABSUIT.mode = FALSE)
               , "The number of data sets requested (`no_simulations`) is too small compared to the number of parameters that must vary."
               , fixed = TRUE)
  
})




## OUTPUTS
test_that("PRE_FATE.params_multipleSet gives error for other conditions / scenarios", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
    if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
    
    PRE_FATE.skeletonDirectory()
    
    file.create("FATE_simulation/DATA/MASK/mask.txt")
    
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_1.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_2.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_3.txt")
    
    file.create("FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    cat(paste0("## Test file\n"
               , "TEST 10000\n")
        , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    
    file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
    cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
               , "--END_OF_FILE--\n")
        , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  }
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = TRUE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = FALSE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The global parameter file indicated in FATE_simulation/PARAM_SIMUL/toto.txt does not contain any of the required parameter values (MAX_ABUND_LOW). Please check."
               , fixed = TRUE)
  
})


## OUTPUTS
test_that("PRE_FATE.params_multipleSet gives error for scenario NO_STRATA", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
    if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
    
    PRE_FATE.skeletonDirectory()
    
    file.create("FATE_simulation/DATA/MASK/mask.txt")
    
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_1.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_2.txt")
    file.create("FATE_simulation/DATA/PFGS/DISP/DISP_3.txt")
    
    file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
    cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
               , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
               , "--END_OF_FILE--\n")
        , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
    
    
    file.create("FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    cat(paste0("## Test file\n"
               , "NO_STRATA 3\n")
        , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
  }
  
  if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The flag --PFG_PARAMS_LIFE_HISTORY-- in the file FATE_simulation/PARAM_SIMUL/toto.txt does not contain any value. Please check.")
  
  cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
             , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
             , "--PFG_PARAMS_LIFE_HISTORY--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
             , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
             , "--END_OF_FILE--\n")
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (./FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt)"
               , fixed = TRUE)
  
  cat(paste0("NAME FG1\nTYPE H\nHEIGHT 15\nMATURITY 2\nLONGEVITY 5\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
  cat(paste0("NAME FG2\nTYPE C\nHEIGHT 150\nMATURITY 5\nLONGEVITY 10\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
  cat(paste0("NAME FG3\nTYPE P\nHEIGHT 1100\nMATURITY 20\nLONGEVITY 80\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 0\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.max_abund_low = FALSE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = TRUE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = FALSE
                                             , do.SOIL.retention = FALSE
                                             , do.DISPERSAL.mode = FALSE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/PFGS/SUCC/REP-1/SUCC_FG1.txt has been successfully created !")
  
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 0\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 1\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "The flag --PFG_PARAMS_LIGHT-- in the file FATE_simulation/PARAM_SIMUL/toto.txt does not contain any value. Please check.")
  
  cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
             , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
             , "--PFG_PARAMS_LIFE_HISTORY--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
             , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
             , "--PFG_PARAMS_LIGHT--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
             , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
             , "--END_OF_FILE--\n")
      , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "`flag` (LIGHT) is not found within `params.lines` (./FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt)"
               , fixed = TRUE)
  
  cat(paste0("NAME FG1\nTYPE H\nHEIGHT 15\nMATURITY 2\nLONGEVITY 5\nLIGHT 1\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
  cat(paste0("NAME FG2\nTYPE C\nHEIGHT 150\nMATURITY 5\nLONGEVITY 10\nLIGHT 3\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
  cat(paste0("NAME FG3\nTYPE P\nHEIGHT 1100\nMATURITY 20\nLONGEVITY 80\nLIGHT 5\n")
      , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "`flag` (LIGHT_THRESH_MEDIUM) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 0\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 1\n"
             , "LIGHT_THRESH_MEDIUM 5\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , do.max_abund_low = FALSE
                                           , do.max_abund_medium = FALSE
                                           , do.max_abund_high = FALSE
                                           , do.seeding_duration = FALSE
                                           , do.seeding_timestep = FALSE
                                           , do.seeding_input = FALSE
                                           , do.no_strata = TRUE
                                           , do.LIGHT.thresh_medium = FALSE
                                           , do.LIGHT.thresh_low = FALSE
                                           , do.SOIL.init = FALSE
                                           , do.SOIL.retention = FALSE
                                           , do.DISPERSAL.mode = FALSE
                                           , do.HABSUIT.mode = FALSE)
               , "`flag` (LIGHT_THRESH_LOW) is not found within `params.lines` (FATE_simulation_MULTIPLE_SET/tmp_global_param.txt)"
               , fixed = TRUE)
  
  cat(paste0("## Test file\n"
             , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
             , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
             , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
             , "DO_DISPERSAL 0\n"
             , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 1\n"
             , "LIGHT_THRESH_MEDIUM 5\nLIGHT_THRESH_LOW 10\n"
             , "DO_SOIL_COMPETITION 0\nDO_DISTURBANCES 0\n")
      , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , do.max_abund_low = FALSE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = TRUE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = FALSE
                                             , do.SOIL.retention = FALSE
                                             , do.DISPERSAL.mode = FALSE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/PFGS/LIGHT/REP-1/LIGHT_FG1.txt has been successfully created !")
  
})


## OUTPUTS
test_that("PRE_FATE.params_multipleSet gives error for scenario SOIL", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
    if (dir.exists("FATE_simulation_MULTIPLE_SET")) unlink("FATE_simulation_MULTIPLE_SET", recursive = TRUE)
    
    PRE_FATE.skeletonDirectory()
    
    file.create("FATE_simulation/DATA/MASK/mask.txt")
    
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
    file.create("FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
    cat(paste0("NAME FG1\nTYPE H\nHEIGHT 15\nMATURITY 2\nLONGEVITY 5\n")
        , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt")
    cat(paste0("NAME FG2\nTYPE C\nHEIGHT 150\nMATURITY 5\nLONGEVITY 10\n")
        , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt")
    cat(paste0("NAME FG3\nTYPE P\nHEIGHT 1100\nMATURITY 20\nLONGEVITY 80\n")
        , file = "FATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt")
    
    cat(paste0("NAME FG1\nTYPE H\nHEIGHT 15\nMATURITY 2\nLONGEVITY 5\n")
        , file = "FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt")
    cat(paste0("NAME FG2\nTYPE C\nHEIGHT 150\nMATURITY 5\nLONGEVITY 10\n")
        , file = "FATE_simulation/DATA/PFGS/SOIL/SOIL_2.txt")
    cat(paste0("NAME FG3\nTYPE P\nHEIGHT 1100\nMATURITY 20\nLONGEVITY 80\n")
        , file = "FATE_simulation/DATA/PFGS/SOIL/SOIL_3.txt")
    
    cat(paste0("## Test file\n"
               , "NO_PFG 3\nNO_STRATA 4\nSIMULATION_DURATION 50\n"
               , "SEEDING_DURATION 10\nSEEDING_TIMESTEP 1\nSEEDING_INPUT 100\n"
               , "MAX_ABUND_LOW 500000\nMAX_ABUND_MEDIUM 600000\nMAX_ABUND_HIGH 700000\n"
               , "DO_DISPERSAL 0\n"
               , "DO_HAB_SUITABILITY 0\nDO_LIGHT_COMPETITION 0\n"
               , "DO_SOIL_COMPETITION 1\nSOIL_INIT 0.1\nSOIL_RETENTION 0.1\n"
               , "DO_DISTURBANCES 0\n")
        , file = "FATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt")
    
    file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
    cat(paste0("--GLOBAL_PARAMS--\nFATE_simulation/DATA/GLOBAL_PARAMETERS/glob.txt\n"
               , "--MASK--\nFATE_simulation/DATA/MASK/mask.txt\n"
               , "--PFG_PARAMS_LIFE_HISTORY--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_1.txt\n"
               , "FATE_simulation/DATA/PFGS/SUCC/SUCC_2.txt\nFATE_simulation/DATA/PFGS/SUCC/SUCC_3.txt\n"
               , "--PFG_PARAMS_SOIL--\nFATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt\n"
               , "FATE_simulation/DATA/PFGS/SOIL/SOIL_2.txt\nFATE_simulation/DATA/PFGS/SOIL/SOIL_3.txt\n"
               , "--END_OF_FILE--\n")
        , file = "FATE_simulation/PARAM_SIMUL/toto.txt")
  }
  

  expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                             , file.simulParam.1 = "toto.txt"
                                             , no_simulations = 10
                                             , opt.percent_soil = 0.9
                                             , do.max_abund_low = FALSE
                                             , do.max_abund_medium = FALSE
                                             , do.max_abund_high = FALSE
                                             , do.seeding_duration = FALSE
                                             , do.seeding_timestep = FALSE
                                             , do.seeding_input = FALSE
                                             , do.no_strata = TRUE
                                             , do.LIGHT.thresh_medium = FALSE
                                             , do.LIGHT.thresh_low = FALSE
                                             , do.SOIL.init = TRUE
                                             , do.SOIL.retention = TRUE
                                             , do.DISPERSAL.mode = FALSE
                                             , do.HABSUIT.mode = FALSE)
                 , "The parameter file FATE_simulation_MULTIPLE_SET/DATA/PFGS/SUCC/REP-1/SUCC_FG1.txt has been successfully created !")
  
})



