library(RFate)
context("PRE_FATE.params_globalParameters() function")

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with missing data", {
  expect_error(PRE_FATE.params_globalParameters()
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(NA)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_globalParameters(1)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters("a")
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.no_PFG", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST required.no_PFG : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation")
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = NA)
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = NULL)
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = "")
               , "`required.no_PFG` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.no_strata", {
  
  ## TEST required.no_strata : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5)
               , "`required.no_strata` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = NA)
               , "`required.no_strata` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = NULL)
               , "`required.no_strata` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = "")
               , "`required.no_strata` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.simul_duration", {
  
  ## TEST required.simul_duration : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = NA)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = NULL)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = "")
               , "`required.simul_duration` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_duration", {
  
  ## TEST required.seeding_duration : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = NA)
               , "`required.seeding_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = NULL)
               , "`required.seeding_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = "")
               , "`required.seeding_duration` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_timestep", {
  
  ## TEST required.seeding_timestep : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = NA)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = NULL)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = "")
               , "`required.seeding_timestep` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_input", {
  
  ## TEST required.seeding_input : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = NA)
               , "`required.seeding_input` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = NULL)
               , "`required.seeding_input` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = "")
               , "`required.seeding_input` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_low", {
  
  ## TEST required.max_abund_low : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = NA)
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = NULL)
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = "")
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = -1)
               , "`required.max_abund_low` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_medium", {
  
  ## TEST required.max_abund_medium : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = NA)
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = NULL)
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = "")
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = -1)
               , "`required.max_abund_medium` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_high", {
  
  ## TEST required.max_abund_high : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = NA)
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = NULL)
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = "")
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = -1)
               , "`required.max_abund_high` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_...", {
  
  ## TEST required.max_abund_medium : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 1
                                                , required.max_abund_high = 9000)
               , "`required.max_abund_low` must contain values equal or inferior to `required.max_abund_medium`")
  
  ## TEST required.max_abund_high : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 1)
               , "`required.max_abund_medium` must contain values equal or inferior to `required.max_abund_high`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : LIGHT.thresh_medium", {
  
  ## TEST LIGHT.thresh_medium : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = NA)
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = NULL)
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = "")
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = -1)
               , "`LIGHT.thresh_medium` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : LIGHT.thresh_low", {
  
  ## TEST LIGHT.thresh_low : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = NA)
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = NULL)
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = "")
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = -1)
               , "`LIGHT.thresh_low` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : LIGHT.thresh_...", {
  
  ## TEST LIGHT.thresh_medium : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 1
                                                , LIGHT.thresh_low = 0)
               , "`LIGHT.thresh_medium` must contain values equal or inferior to `LIGHT.thresh_low`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : SOIL.init", {
  
  ## TEST SOIL.init : numeric values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = NA)
               , "`SOIL.init` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = NULL)
               , "`SOIL.init` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = "")
               , "`SOIL.init` must contain numeric values")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : SOIL.retention", {
  
  ## TEST SOIL.retention : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = 0
                                                , SOIL.retention = NA)
               , "`SOIL.retention` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = 0
                                                , SOIL.retention = NULL)
               , "`SOIL.retention` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = 0
                                                , SOIL.retention = "")
               , "`SOIL.retention` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = 0
                                                , SOIL.retention = -0.2)
               , "`SOIL.retention` must contain values between `0` and `1`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doSoil = T
                                                , SOIL.init = 0
                                                , SOIL.retention = 10)
               , "`SOIL.retention` must contain values between `0` and `1`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DISPERSAL.mode", {
  
  ## TEST DISPERSAL.mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDispersal = T
                                                , DISPERSAL.mode = NA)
               , "`DISPERSAL.mode` must be either `1`, `2` or `3`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDispersal = T
                                                , DISPERSAL.mode = NULL)
               , "`DISPERSAL.mode` must be either `1`, `2` or `3`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDispersal = T
                                                , DISPERSAL.mode = "")
               , "`DISPERSAL.mode` must be either `1`, `2` or `3`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDispersal = T
                                                , DISPERSAL.mode = factor("a"))
               , "`DISPERSAL.mode` must be either `1`, `2` or `3`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : HABSUIT.mode", {
  
  ## TEST HABSUIT.mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doHabSuitability = T
                                                , HABSUIT.mode = NA)
               , "`HABSUIT.mode` must be either `1` or `2`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doHabSuitability = T
                                                , HABSUIT.mode = NULL)
               , "`HABSUIT.mode` must be either `1` or `2`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doHabSuitability = T
                                                , HABSUIT.mode = "")
               , "`HABSUIT.mode` must be either `1` or `2`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doHabSuitability = T
                                                , HABSUIT.mode = factor("a"))
               , "`HABSUIT.mode` must be either `1` or `2`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.no", {
  
  ## TEST DIST.no : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = NA)
               , "`DIST.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = NULL)
               , "`DIST.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = "")
               , "`DIST.no` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.no_sub", {
  
  ## TEST DIST.no_sub : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = NA)
               , "`DIST.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = NULL)
               , "`DIST.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = "")
               , "`DIST.no_sub` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.freq", {
  
  ## TEST DIST.freq : integer (vector)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2)
               , "`DIST.freq` must contain as many values as the number of disturbances (`DIST.no`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = NA)
               , "`DIST.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = NULL)
               , "`DIST.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = "")
               , "`DIST.freq` must be an integer > 0")
  
  ## TEST DIST.freq : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = 2)
               , "`DIST.freq` must contain as many values as the number of disturbances (`DIST.no`)"
               , fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DROUGHT.no_sub", {
  
  ## TEST DROUGHT.no_sub : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDrought = T
                                                , DROUGHT.no_sub = NA)
               , "`DROUGHT.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDrought = T
                                                , DROUGHT.no_sub = NULL)
               , "`DROUGHT.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doDrought = T
                                                , DROUGHT.no_sub = "")
               , "`DROUGHT.no_sub` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : ALIEN.no", {
  
  ## TEST ALIEN.no : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = NA)
               , "`ALIEN.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = NULL)
               , "`ALIEN.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = "")
               , "`ALIEN.no` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : ALIEN.freq", {
  
  ## TEST ALIEN.freq : integer (vector)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = 2)
               , "`ALIEN.freq` must contain as many values as the number of introductions (`ALIEN.no`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = 2
                                                , ALIEN.freq = NA)
               , "`ALIEN.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = 2
                                                , ALIEN.freq = NULL)
               , "`ALIEN.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = 2
                                                , ALIEN.freq = "")
               , "`ALIEN.freq` must be an integer > 0")
  
  ## TEST ALIEN.freq : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doAliens = T
                                                , ALIEN.no = 2
                                                , ALIEN.freq = 2)
               , "`ALIEN.freq` must contain as many values as the number of introductions (`ALIEN.no`)"
               , fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.no", {
  
  ## TEST FIRE.no : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = NA)
               , "`FIRE.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = NULL)
               , "`FIRE.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = "")
               , "`FIRE.no` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.no_sub", {
  
  ## TEST FIRE.no_sub : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = NA)
               , "`FIRE.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = NULL)
               , "`FIRE.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = "")
               , "`FIRE.no_sub` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.freq", {
  
  ## TEST FIRE.freq : integer (vector)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2)
               , "`FIRE.freq` must contain as many values as the number of disturbances (`FIRE.no`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = NA)
               , "`FIRE.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = NULL)
               , "`FIRE.freq` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = "")
               , "`FIRE.freq` must be an integer > 0")
  
  ## TEST FIRE.freq : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2)
               , "`FIRE.freq` must contain as many values as the number of disturbances (`FIRE.no`)"
               , fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.ignit_mode", {
  
  ## TEST FIRE.ignit_mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = NA)
               , "`FIRE.ignit_mode` must be either `1`, `2`, `3`, `4` or `5`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = NULL)
               , "`FIRE.ignit_mode` must be either `1`, `2`, `3`, `4` or `5`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = "")
               , "`FIRE.ignit_mode` must be either `1`, `2`, `3`, `4` or `5`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = factor("a"))
               , "`FIRE.ignit_mode` must be either `1`, `2`, `3`, `4` or `5`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.ignit_no", {
  
  ## TEST FIRE.ignit_no : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 1)
               , "`FIRE.ignit_no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 1
                                                , FIRE.ignit_no = NULL)
               , "`FIRE.ignit_no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 1
                                                , FIRE.ignit_no = "")
               , "`FIRE.ignit_no` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.ignit_noHist", {
  
  ## TEST FIRE.ignit_noHist : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 3)
               , "`FIRE.ignit_noHist` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 3
                                                , FIRE.ignit_noHist = NULL)
               , "`FIRE.ignit_noHist` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 3
                                                , FIRE.ignit_noHist = "")
               , "`FIRE.ignit_noHist` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.ignit_logis", {
  
  ## TEST FIRE.ignit_logis : numeric
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = NULL)
               , "`FIRE.ignit_logis` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = "")
               , "`FIRE.ignit_logis` must contain numeric values")
  
  ## TEST FIRE.ignit_logis : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = 2)
               , "`FIRE.ignit_logis` must contain 3 numeric values")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.ignit_flammMax", {
  
  ## TEST FIRE.ignit_flammMax : numeric
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = 1:3)
               , "`FIRE.ignit_flammMax` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = 1:3
                                                , FIRE.ignit_flammMax = NULL)
               , "`FIRE.ignit_flammMax` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 4
                                                , FIRE.ignit_logis = 1:3
                                                , FIRE.ignit_flammMax = "")
               , "`FIRE.ignit_flammMax` must contain numeric values")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.neigh_mode", {
  
  ## TEST FIRE.neigh_mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = NULL)
               , "`FIRE.neigh_mode` must be either `1`, `2` or `3`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = "")
               , "`FIRE.neigh_mode` must be either `1`, `2` or `3`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = factor("a"))
               , "`FIRE.neigh_mode` must be either `1`, `2` or `3`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.neigh_CC", {
  
  ## TEST FIRE.neigh_CC : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 2
                                                , FIRE.neigh_CC = NULL)
               , "`FIRE.neigh_CC` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 2
                                                , FIRE.neigh_CC = "")
               , "`FIRE.neigh_CC` must be an integer > 0")
  
  ## TEST FIRE.neigh_CC : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 2
                                                , FIRE.neigh_CC = 2)
               , "`FIRE.neigh_CC` must contain 4 numeric values")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.prop_mode", {
  
  ## TEST FIRE.prop_mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = NULL)
               , "`FIRE.prop_mode` must be either `1`, `2`, `3`, `4` or `5`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = "")
               , "`FIRE.prop_mode` must be either `1`, `2`, `3`, `4` or `5`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = factor("a"))
               , "`FIRE.prop_mode` must be either `1`, `2`, `3`, `4` or `5`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.prop_intensity", {
  
  ## TEST FIRE.prop_intensity : numeric
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 1)
               , "`FIRE.prop_intensity` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 1
                                                , FIRE.prop_intensity = NULL)
               , "`FIRE.prop_intensity` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 1
                                                , FIRE.prop_intensity = "")
               , "`FIRE.prop_intensity` must contain numeric values")
  
  ## TEST FIRE.prop_intensity : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 1
                                                , FIRE.prop_intensity = 1.5)
               , "`FIRE.prop_intensity` must contain values between `0` and `1`")
  
  ## TEST FIRE.prop_intensity : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 2
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = c(1,2)
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 1
                                                , FIRE.prop_intensity = 1)
               , "`FIRE.prop_intensity` must contain as many values as the number of disturbances (`FIRE.no`)"
               , fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.prop_logis", {
  
  ## TEST FIRE.prop_logis : numeric
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 5
                                                , FIRE.prop_logis = NULL)
               , "`FIRE.prop_logis` must contain numeric values")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 5
                                                , FIRE.prop_logis = "")
               , "`FIRE.prop_logis` must contain numeric values")
  
  ## TEST FIRE.prop_logis : correct number of values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 5
                                                , FIRE.prop_logis = 2)
               , "`FIRE.prop_logis` must contain 3 numeric values")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.quota_mode", {
  
  ## TEST FIRE.quota_mode : correct values
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = NULL)
               , "`FIRE.quota_mode` must be either `1`, `2`, `3` or `4`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = "")
               , "`FIRE.quota_mode` must be either `1`, `2`, `3` or `4`")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = factor("a"))
               , "`FIRE.quota_mode` must be either `1`, `2`, `3` or `4`")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : FIRE.quota_max", {
  
  ## TEST FIRE.quota_max : integer
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = 1
                                                , FIRE.quota_max = NA)
               , "`FIRE.quota_max` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = 1
                                                , FIRE.quota_max = NULL)
               , "`FIRE.quota_max` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_strata = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000
                                                , required.max_abund_medium = 5000
                                                , required.max_abund_high = 9000
                                                , doFire = T
                                                , FIRE.no = 1
                                                , FIRE.no_sub = 2
                                                , FIRE.freq = 2
                                                , FIRE.ignit_mode = 5
                                                , FIRE.neigh_mode = 1
                                                , FIRE.prop_mode = 2
                                                , FIRE.quota_mode = 1
                                                , FIRE.quota_max = "")
               , "`FIRE.quota_max` must be an integer > 0")
})




## OUTPUTS
test_that("PRE_FATE.params_globalParameters gives correct output : scenario classic", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.no_CPU = "a"
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000)
                 , "`opt.no_CPU` must be an integer > 0"
                 , fixed = TRUE)
  
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V2.txt has been successfully created !")
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000)
                 , "already exists. It will be replaced.")
})



## OUTPUTS
test_that("PRE_FATE.params_globalParameters gives correct output : scenario modules", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## Light
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doLight = TRUE
                                                  , LIGHT.thresh_medium = 4000
                                                  , LIGHT.thresh_low = 7000)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Soil
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doSoil = TRUE
                                                  , SOIL.init = 0
                                                  , SOIL.retention = 0.8)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Dispersal
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doDispersal = TRUE
                                                  , DISPERSAL.mode = 2)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Habitat suitability
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doHabSuitability = TRUE
                                                  , HABSUIT.mode = 1)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Disturbances
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doDisturbances = TRUE
                                                  , DIST.no = 2
                                                  , DIST.no_sub = 2
                                                  , DIST.freq = c(2,2))
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Drought
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doDrought = TRUE
                                                  , DROUGHT.no_sub = 3)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Aliens
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doAliens = TRUE
                                                  , ALIEN.no = 10
                                                  , ALIEN.freq = rep(1, 10))
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
  ## Fire
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doFire = TRUE
                                                  , FIRE.no = 2
                                                  , FIRE.no_sub = 2
                                                  , FIRE.freq = c(2,2)
                                                  , FIRE.ignit_mode = 1
                                                  , FIRE.ignit_no = 10
                                                  , FIRE.neigh_mode = 2
                                                  , FIRE.neigh_CC = c(3,4,3,4)
                                                  , FIRE.prop_mode = 1
                                                  , FIRE.prop_intensity = c(0.3, 0.7)
                                                  , FIRE.quota_mode = 1
                                                  , FIRE.quota_max = 20)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  
})



## OUTPUTS
test_that("PRE_FATE.params_globalParameters gives correct output : warning round", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000.5
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000)
                 , "`required.max_abund_low` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000.5
                                                  , required.max_abund_high = 9000)
                 , "`required.max_abund_medium` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000.5)
                 , "`required.max_abund_high` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doLight = TRUE
                                                  , LIGHT.thresh_medium = 10.5
                                                  , LIGHT.thresh_low = 50)
                 , "`LIGHT.thresh_medium` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_strata = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000
                                                  , required.max_abund_medium = 5000
                                                  , required.max_abund_high = 9000
                                                  , doLight = TRUE
                                                  , LIGHT.thresh_medium = 10
                                                  , LIGHT.thresh_low = 50.5)
                 , "`LIGHT.thresh_low` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
})
