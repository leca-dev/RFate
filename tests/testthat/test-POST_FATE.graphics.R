library(RFate)
context("POST_FATE.graphics() function")


## INPUTS
test_that("POST_FATE.graphics gives error with missing data", {
  expect_error(POST_FATE.graphics()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphics(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})


## OUTPUTS
test_that("POST_FATE.graphics gives correct outputs :", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    map_hab = map_0
    map_hab[] = sample(c(1, 5, 10), ncell(map_hab), replace = TRUE)
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    writeRaster(map_hab, filename = "FATE_simulation/DATA/MASK/map_hab.tif", overwrite = TRUE)
    
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
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata")
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
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/"
                                           , "Binary_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata/"
                                           , "Binary_YEAR_1_PFG", i, "_STRATA_1.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata/"
                                           , "Binary_YEAR_1_PFG", i, "_STRATA_2.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_1.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/LIGHT/"
                                         , "Light_Resources_YEAR_1_STRATA_1.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/SOIL/"
                                         , "Soil_Resources_YEAR_1.tif"), overwrite = TRUE)
    
    mat.PFG.obs = xyFromCell(map_0, 1:ncell(map_0))
    mat.PFG.obs = expand.grid(PFG = paste0("PFG",1:6)
                              , X = mat.PFG.obs[, 1]
                              , Y = mat.PFG.obs[, 2])
    mat.PFG.obs$obs = sample(c(0, 1), nrow(mat.PFG.obs), prob = c(0.6, 0.4), replace = TRUE)
  }
  
  
  simulGraphics = POST_FATE.graphics(name.simulation = "FATE_simulation"
                                     , years = 1
                                     , no_years = 10
                                     , opt.ras_habitat = "FATE_simulation/DATA/MASK/map_hab.tif"
                                     , doFunc.evolCov = TRUE
                                     , doFunc.evolPix = TRUE
                                     , doFunc.evolStab = TRUE
                                     , doFunc.valid = TRUE
                                     , valid.mat.PFG.obs = mat.PFG.obs
                                     , doFunc.mapPFGvsHS = TRUE
                                     , doFunc.mapPFG = TRUE
                                     , mapPFGvsHS.stratum = 2
                                     , binMap.method = 1
                                     , mapPFG.doBinary = TRUE
                                     , opt.doPlot = TRUE)

  expect_output(str(simulGraphics), "List")
  expect_equal(length(simulGraphics), 1)

  expect_output(str(simulGraphics[[1]]), "List")
  expect_equal(length(simulGraphics[[1]]), 6)
})
