library(RFate)
context("PRE_FATE.params_changingYears() function")

## INPUTS
test_that("PRE_FATE.params_changingYears gives error with missing data", {
  expect_error(PRE_FATE.params_changingYears()
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(NA)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
})


## INPUTS
test_that("PRE_FATE.params_changingYears gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_changingYears(1)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears("a")
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
})


## INPUTS
test_that("PRE_FATE.params_changingYears gives error with wrong data : type.changing", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST type.changing : correct values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation")
               , "`type.changing` must be either `MASK`, `HABSUIT`, `DIST`, `DROUGHT`, `ALIENS`, `ALIENS_F`, `FIRE` or `FIRE_F`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = NULL)
               , "`type.changing` must be either `MASK`, `HABSUIT`, `DIST`, `DROUGHT`, `ALIENS`, `ALIENS_F`, `FIRE` or `FIRE_F`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = NA)
               , "`type.changing` must be either `MASK`, `HABSUIT`, `DIST`, `DROUGHT`, `ALIENS`, `ALIENS_F`, `FIRE` or `FIRE_F`"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "dist")
               , "`type.changing` must be either `MASK`, `HABSUIT`, `DIST`, `DROUGHT`, `ALIENS`, `ALIENS_F`, `FIRE` or `FIRE_F`"
               , fixed = TRUE)
})


## INPUTS
test_that("PRE_FATE.params_changingYears gives error with wrong data : mat.changing", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()

  ## TEST mat.changing : data.frame
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST")
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = NA)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = NULL)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = "")
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = 1)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = factor(1))
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = matrix(1))
               , "`mat.changing` must be a data.frame")
  
  
  ## TEST mat.changing : correct number of rows and columns
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame())
               , "`mat.changing` does not have the appropriate number of rows (>0) or columns (year, order, new.value)"
               , fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(1))
               , "`mat.changing` does not have the appropriate number of rows (>0) or columns (year, order, new.value)"
               , fixed = TRUE)
  
  ## TEST mat.changing : correct names of columns
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(1,2,3))
               , "Column names of `mat.changing` must be `year`, `order` and `new.value`")
  
  
  ## TEST mat.changing$year : numeric values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = NA, order = 1, new.value = "hop"))
               , "`mat.changing$year` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = "a", order = 1, new.value = "hop"))
               , "`mat.changing$year` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = factor(1), order = 1, new.value = "hop"))
               , "`mat.changing$year` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.changing$year : no NA values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = c(10,NA), order = 1, new.value = "hop"))
               , "`mat.changing$year` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.changing$order : numeric values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = NA, new.value = "hop"))
               , "`mat.changing$order` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = "a", new.value = "hop"))
               , "`mat.changing$order` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = factor(1), new.value = "hop"))
               , "`mat.changing$order` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.changing$order : no NA values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = c(1,NA), new.value = "hop"))
               , "`mat.changing$order` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.changing$new.value : length > 0
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = 1, new.value = ""))
               , "`mat.changing$new.value` must contain a character value of length > 0", fixed = TRUE)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = 1, new.value = c("",NA)))
               , "`mat.changing$new.value` must contain a character value of length > 0", fixed = TRUE)
  
  ## TEST mat.changing$new.value : no NA values
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = 1, new.value = c("a",NA)))
               , "`mat.changing$new.value` must not contain NA values", fixed = TRUE)
  
  
  ## TEST mat.changing : year / order balanced
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                             , type.changing = "DIST"
                                             , mat.changing = data.frame(year = c(10,10,50)
                                                                         , order = c(1,2,1)
                                                                         , new.value = c(1,2,3)))
               , "Columns `year` and `order` are not balanced\n All combinations must be represented")
})



## OUTPUTS
test_that("PRE_FATE.params_changingYears gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B")))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changingmask_years.txt has been successfully created !")
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B")))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changingmask_files_t10.txt has been successfully created !")
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B"))
                                               , opt.folder.name = "")
                 , "already exists. It will be replaced.")
  
  
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B"))
                                               , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B"))
                                               , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B"))
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SCENARIO/scen1/DIST_changingmask_years.txt has been successfully created !")
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c("A","B"))
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SCENARIO/scen1/DIST_changingmask_files_t10.txt has been successfully created !")

  
  
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "ALIENS_F"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c(1,2)))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/ALIENS_changingfreq_years.txt has been successfully created !")
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                               , type.changing = "ALIENS_F"
                                               , mat.changing = data.frame(year = c(10,10)
                                                                           , order = c(1,2)
                                                                           , new.value = c(1,2)))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/ALIENS_changingfreq_files_t10.txt has been successfully created !")
  
})
