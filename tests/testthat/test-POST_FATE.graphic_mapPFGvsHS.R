library(RFate)
context("POST_FATE.graphic_mapPFGvsHS() function")


## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with missing data", {
  expect_error(POST_FATE.graphic_mapPFGvsHS()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGvsHS(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGvsHS(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_mapPFGvsHS(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGvsHS("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGvsHS(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGvsHS(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with wrong data : years", {
  
  ## TEST years : integer
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt"
                                            , years = "a")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt"
                                            , years = factor("a"))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt"
                                            , years = factor(1))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt"
                                            , years = NULL)
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , file.simulParam = "ParamSimul.txt"
                                            , years = NA)
               , "`years` must be an integer > 0")
})



## INPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives error with wrong data : rasters", {
  {
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
  }
  
  ## TEST RESULTS folder
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation", years = 1)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                            , years = 1
                                            , opt.stratum = 1)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata/ does not contain adequate files")
  
  
  file.create("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/Binary_YEAR_1_PFG1_STRATA_all.tif")
  # expect_error(POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation", years = 1)
  #              , "Cannot create a RasterLayer object from this file.")
  
})


## OUTPUTS
test_that("POST_FATE.graphic_mapPFGvsHS gives correct outputs :", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    map_hab = map_0
    map_hab[] = runif(ncell(map_hab))
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    writeRaster(map_hab, filename = "FATE_simulation/DATA/MASK/map_hab.tif", overwrite = TRUE)
    
    for (i in 1:6)
    {
      writeRaster(map_hab, filename = paste0("FATE_simulation/DATA/PFGS/HABSUIT/"
                                             , "HABSUIT_PFG", i, ".tif"), overwrite = TRUE)
    }
    
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
                                     , doHabSuitability = TRUE
                                     , HABSUIT.mode = 1)
    
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
    
    for (i in 1:5)
    {
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_1.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_1.tif"), overwrite = TRUE)
    POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
    
    mat.PFG.obs = xyFromCell(map_0, 1:ncell(map_0))
    mat.PFG.obs = expand.grid(PFG = paste0("PFG",1:6)
                              , X = mat.PFG.obs[, 1]
                              , Y = mat.PFG.obs[, 2])
    mat.PFG.obs$obs = sample(c(0, 1), nrow(mat.PFG.obs), prob = c(0.6, 0.4), replace = TRUE)
    validStat = POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                       , years = 1
                                                       , mat.PFG.obs = mat.PFG.obs
                                                       , opt.doPlot = FALSE)
    POST_FATE.binaryMaps(name.simulation = "FATE_simulation", years = 1, method = 1)
  }
  
  PFGvsHS = POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                         , years = 1
                                         , opt.stratum = "all")
  
  expect_output(str(PFGvsHS), "List")
  expect_equal(length(PFGvsHS), 1)
  
  PFGvsHS = POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
                                         , years = 1
                                         , opt.stratum = 1)

  expect_output(str(PFGvsHS), "List")
  expect_equal(length(PFGvsHS), 1)
  
  expect_output(str(PFGvsHS[[1]]), "List")
  expect_equal(length(PFGvsHS[[1]]), 1)
  expect_output(str(PFGvsHS[[1]][[1]]), "List")
  expect_equal(length(PFGvsHS[[1]][[1]]), 6)
})

