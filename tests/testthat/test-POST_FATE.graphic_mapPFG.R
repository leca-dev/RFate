library(RFate)
context("POST_FATE.graphic_mapPFG() function")

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with missing data", {
  expect_error(POST_FATE.graphic_mapPFG()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFG(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFG(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_mapPFG(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFG("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFG(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFG(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : years", {
  
  ## TEST years : integer
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = "a")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = factor("a"))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = factor(1))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = NULL)
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = NA)
               , "`years` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : opt.stratum_min", {
  
  ## TEST opt.stratum_min : integer
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = "a")
               , "`opt.stratum_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = factor("a"))
               , "`opt.stratum_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = factor(1))
               , "`opt.stratum_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = NULL)
               , "`opt.stratum_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = NA)
               , "`opt.stratum_min` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : opt.stratum_max", {
  
  ## TEST opt.stratum_max : integer
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_max = "a")
               , "`opt.stratum_max` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_max = factor("a"))
               , "`opt.stratum_max` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_max = factor(1))
               , "`opt.stratum_max` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_max = NULL)
               , "`opt.stratum_max` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_max = NA)
               , "`opt.stratum_max` must be an integer > 0")
  
  ## TEST opt.stratum_max : correct value
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                        , file.simulParam = "ParamSimul.txt"
                                        , years = 1
                                        , opt.stratum_min = 3
                                        , opt.stratum_max = 2)
               , "`opt.stratum_min` must contain value equal or inferior to `opt.stratum_max`")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFG gives error with wrong data : rasters", {
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
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:6)
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
  
  
  ## TEST RESULTS folder : opt.doBinary = TRUE, opt.doStrata = FALSE
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/Binary_YEAR_1_PFG1_STRATA_all.tif")
  
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif")
  # expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1)
  #              , "Cannot create a RasterLayer object from this file.")
  
  
  ## TEST RESULTS folder : opt.doBinary = FALSE, opt.doStrata = TRUE
  unlink("FATE_simulation/RESULTS/SIMUL_V1", recursive = TRUE)
  dir.create("FATE_simulation/RESULTS/SIMUL_V1")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
  
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1
                                        , opt.doBinary = FALSE, opt.stratum_min = 2)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/Abund_YEAR_1_PFG1_STRATA_1.tif")
  expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1
                                        , opt.doBinary = FALSE, opt.stratum_min = 2)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/Abund_YEAR_1_PFG1_STRATA_2.tif")
  # expect_error(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation", years = 1
  #                                       , opt.doBinary = FALSE, opt.stratum_min = 2)
  #              , "Cannot create a RasterLayer object from this file.")
})




## OUTPUTS
test_that("POST_FATE.graphic_mapPFG gives correct outputs :", {
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
                                     , required.max_abund_high = 9000
                                     , doLight = TRUE
                                     , LIGHT.thresh_medium = 4000
                                     , LIGHT.thresh_low = 7000
                                     , doSoil = TRUE
                                     , SOIL.init = 2.5
                                     , SOIL.retention = 0.5)
    
    PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                  , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:6)
                                                              , type = c("C", "C", "H", "H", "P", "P")
                                                              , height = c(10, 250, 36, 68, 1250, 550)
                                                              , maturity = c(5, 5, 3, 3, 8, 9)
                                                              , longevity = c(12, 200, 25, 4, 110, 70)))
    
    PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                             , mat.PFG.light = data.frame(PFG = paste0("PFG",1:6)
                                                          , type = c("C", "C", "H", "H", "P", "P"))
                             , mat.PFG.tol = data.frame(PFG = paste0("PFG",1:6)
                                                        , strategy_tol = c("ubiquist", "undergrowth"
                                                                           , "ubiquist", "semi_shade"
                                                                           , "pioneer", "full_light")))
    
    PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                            , mat.PFG.soil = data.frame(PFG = paste0("PFG",1:6)
                                                        , type = c("C", "C", "H", "H", "P", "P")
                                                        , soil_contrib = c(2, 2.5, 3.5, 3, 1.5, 3)
                                                        , soil_tol_min = c(1, 2, 2, 1.5, 1, 2)
                                                        , soil_tol_max = c(3, 3, 5, 4.5, 2, 4)))
    
    PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                    , name.MASK = "map_mask.tif")
    
    dir.create("FATE_simulation/RESULTS/SIMUL_V1")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
    
    for (i in 1:5)
    {
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_1.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_2.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_1.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/LIGHT/"
                                         , "Light_Resources_YEAR_1_STRATA_1.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/SOIL/"
                                         , "Soil_Resources_YEAR_1.tif"), overwrite = TRUE)
    
    POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
    POST_FATE.binaryMaps(name.simulation = "FATE_simulation", years = 1, method = 1)
  }
  
  ## TEST opt.doBinary = TRUE
  expect_message(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = TRUE)
                 , "> FATE_simulation/RESULTS/SIMUL_V1/PFGcover_YEAR_1_STRATA_all.tif")
  expect_warning(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = TRUE)
                 , "do not contain `LIGHT` flag parameter. Please check.")
  
  PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                           , mat.PFG.light = data.frame(PFG = paste0("PFG",1:6)
                                                        , type = c("C", "C", "H", "H", "P", "P")
                                                        , light_need = c(3, 1, 3, 2, 5, 4)))
  
  expect_message(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = TRUE)
                 , "> FATE_simulation/RESULTS/SIMUL_V1/PFGlight_YEAR_1_STRATA_all.tif")
  
  ## TEST opt.doBinary = TRUE, opt.doStrata = TRUE
  expect_message(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = TRUE
                                          , opt.stratum_min = 2)
                 , "> FATE_simulation/RESULTS/SIMUL_V1/PFGcover_YEAR_1_STRATA_2_5.tif")
  
  ## TEST opt.doBinary = FALSE
  expect_message(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = FALSE)
                 , "> FATE_simulation/RESULTS/SIMUL_V1/PFGcover_YEAR_1_STRATA_all.tif")
  
  ## TEST opt.doBinary = FALSE, opt.doStrata = TRUE
  expect_message(POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
                                          , years = 1, opt.doBinary = FALSE
                                          , opt.stratum_min = 2)
                 , "> FATE_simulation/RESULTS/SIMUL_V1/PFGcover_YEAR_1_STRATA_2_5.tif")
})

