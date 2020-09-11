library(RFate)
context("PRE_FATE.params_PFGdispersal() function")

## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with missing data", {
  expect_error(PRE_FATE.params_PFGdispersal()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGdispersal(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with wrong data : mat.PFG.disp", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST mat.PFG.disp : data.frame
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation")
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = NA)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = NULL)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = "")
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = 1)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = factor(1))
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = matrix(1))
               , "`mat.PFG.disp` must be a data.frame")
  
  ## TEST mat.PFG.disp :correct number of rows and columns
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame())
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1))
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.disp : correct names of columns
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1, 2, 3, 4))
               , "Column names of `mat.PFG.disp` must be `PFG`, `d50`, `d99` and `ldd`")
  
  ## TEST mat.PFG.disp$PFG : different values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = c(2,2), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain different values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$PFG : length > 0
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = "", d50 = NA, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain a character value of length > 0", fixed = TRUE)
  

  ## TEST mat.PFG.disp$d50 : numeric values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = NA, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = "a", d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor(1), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor("a"), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$d50 : no NA values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = c(2,NA), d99 = 3, ldd = 4))
               , "`mat.PFG.disp$d50` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.PFG.disp$d99 : numeric values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = NA, ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = "a", ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor(1), ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor("a"), ldd = 4))
               , "`mat.PFG.disp$d99` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$d99 : no NA values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = c(3,NA), ldd = 4))
               , "`mat.PFG.disp$d99` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$ldd : numeric values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = NA))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = "a"))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor(1)))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor("a")))
               , "`mat.PFG.disp$ldd` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.disp$ldd : no NA values
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = 3, ldd = c(4,NA)))
               , "`mat.PFG.disp$ldd` must not contain NA values", fixed = TRUE)
})



## OUTPUTS
test_that("PRE_FATE.params_PFGdispersal gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4))
                 , "The parameter file FATE_simulation/DATA/PFGS/DISP/DISP_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = "")
                 , "already exists. It will be replaced.")
  
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/DISP/scen1/DISP_1.txt has been successfully created !")
})

