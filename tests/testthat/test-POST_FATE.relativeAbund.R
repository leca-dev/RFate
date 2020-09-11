library(RFate)
context("POST_FATE.relativeAbund() function")


## INPUTS
test_that("POST_FATE.relativeAbund gives error with missing data", {
  expect_error(POST_FATE.relativeAbund()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.relativeAbund(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : years", {
  
  ## TEST years : integer
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , years = "a")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , years = factor("a"))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , years = factor(1))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , years = NULL)
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , years = NA)
               , "`years` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : rasters", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  library(raster)
  map_mask = raster(nrows = 5, ncols = 5)
  map_mask[] = 1
  writeRaster(map_mask, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
  
  PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                   , required.no_PFG = 6
                                   , required.no_strata = 5
                                   , required.simul_duration = 100
                                   , required.seeding_duration = 10
                                   , required.seeding_timestep = 1
                                   , required.seeding_input = 100
                                   , required.max_abund_low = 3000
                                   , required.max_abund_medium = 5000
                                   , required.max_abund_high = 9000)
  
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
  
  PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                  , name.MASK = "map_mask.tif")
  
  dir.create("FATE_simulation/RESULTS/SIMUL_V1")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
  
  
  ## TEST RESULTS folder
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , years = 10)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/ does not contain adequate files")
  
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif")
  # expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
  #                                      , years = 10)
  #              , "Cannot create a RasterLayer object from this file.")
  
  writeRaster(map_mask, filename = "FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif", overwrite = TRUE)
})




## OUTPUTS
test_that("POST_FATE.relativeAbund gives correct outputs : abundance", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    
    PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                     , required.no_PFG = 6
                                     , required.no_strata = 5
                                     , required.simul_duration = 100
                                     , required.seeding_duration = 10
                                     , required.seeding_timestep = 1
                                     , required.seeding_input = 100
                                     , required.max_abund_low = 3000
                                     , required.max_abund_medium = 5000
                                     , required.max_abund_high = 9000)
    
    PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                  , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                              , type = c("C", "C", "H", "H", "P", "P")
                                                              , height = c(10, 250, 36, 68, 1250, 550)
                                                              , maturity = c(5, 5, 3, 3, 8, 9)
                                                              , longevity = c(12, 200, 25, 4, 110, 70)))
    
    PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                    , name.MASK = "map_mask.tif")
    
    dir.create("FATE_simulation/RESULTS/SIMUL_V1")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
  }
  
  ## TEST .tif files
  for (i in 1:6)
  {
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
  }
  expect_message(POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
                 , "> Abund_relative_YEAR_1_PFG1_STRATA_all.tif")
  
  ## TEST .img files
  for (i in 1:6)
  {
    file.remove(paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                       , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"))
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG", i, "_STRATA_all.img"), overwrite = TRUE)
  }
  expect_message(POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
                 , "> Abund_relative_YEAR_1_PFG1_STRATA_all.tif")
  
  ## TEST .asc files
  for (i in 1:6)
  {
    file.remove(paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                       , "Abund_YEAR_1_PFG", i, "_STRATA_all.img"))
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG", i, "_STRATA_all.asc"), overwrite = TRUE)
  }
  expect_message(POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
                 , "> Abund_relative_YEAR_1_PFG1_STRATA_all.tif")
})
