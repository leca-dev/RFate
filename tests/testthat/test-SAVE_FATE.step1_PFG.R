library(RFate)
context("SAVE_FATE.step1_PFG() function")

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with missing values", {
  expect_error(SAVE_FATE.step1_PFG()
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(NULL)
               , "`name.dataset` must contain a character value of length > 0")
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : name.dataset", {
  expect_error(SAVE_FATE.step1_PFG(name.dataset = NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = 1)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = factor("a"))
               , "`name.dataset` must contain a character value of length > 0")
})



## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : mat.observations", {
  
  ## TEST mat.observations : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = matrix(1))
               , "`mat.observations` must be a data.frame")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = c(1, 89, 3))
               , "`mat.observations` must be a data.frame")
  
  ## TEST mat.observations : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame())
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3,4,5,6,7))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  
  ## TEST mat.observations : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3))
               , "Column names of `mat.observations` must be `sites`, `(x)`, `(y)`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3,4))
               , "Column names of `mat.observations` must be `sites`, `(x)`, `(y)`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  
  ## TEST mat.observations$x : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , x = "a"
                                                                                     , y = 2))
               , "`mat.observations$x` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , x = factor(1)
                                                                                     , y = 2))
               , "`mat.observations$x` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.observations$y : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , y = "a"
                                                                                     , x = 2))
               , "`mat.observations$y` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , y = factor(1)
                                                                                     , x = 2))
               , "`mat.observations$y` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.observations$abund : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = "a"))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = factor(1)))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
})


## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : doRuleA / doRuleB / doRuleC", {
  
  ## TEST doRuleA : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = TRUE
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleA']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST doRuleB : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleB']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST doRuleC : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleC']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST rule.A1 : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['rule.A1']` must contain numeric values"
               , fixed = TRUE)
  
  
  ## TEST rule.A2_quantile : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = 10
                                                              , "rule.A2_quantile" = "a"))
               , "`rules.selectDominant['rule.A2_quantile']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = 10
                                                              , "rule.A2_quantile" = 1.5))
               , "`rules.selectDominant['rule.A2_quantile']` must contain values between `0` and `1`"
               , fixed = TRUE)
  
  ## TEST rule.B1_number : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = "a"))
               , "`rules.selectDominant['rule.B1_number']` must contain numeric values"
               , fixed = TRUE)
  
  ## TEST rule.B1_percentage : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = "a"))
               , "`rules.selectDominant['rule.B1_percentage']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = 1.5))
               , "`rules.selectDominant['rule.B1_percentage']` must contain values between `0` and `1`"
               , fixed = TRUE)
  
  
  ## TEST rule.B2 : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = 0.5
                                                              , "rule.B2" = "a"))
               , "`rules.selectDominant['rule.B2']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" =  0.5
                                                              , "rule.B2" = 1.5))
               , "`rules.selectDominant['rule.B2']` must contain values between `0` and `1`"
               , fixed = TRUE)
})



## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : mat.traits", {
  
  ## TEST mat.traits : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = NA)
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = list(1,NA))
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  
  ## TEST mat.traits : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame())
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(1))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(1,2))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(1, 2, 3))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  
  ## TEST mat.traits : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(c(1,1), 2, 3))
               , "Column names of `mat.traits` must be `species`, `(GROUP)`, `(trait1)`, `(trait2)` and `...`"
               , fixed = TRUE)
  
  
  ## TEST mat.traits$species : different values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   , sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c(1,1), GROUP = 1, 2, 3))
               , "`mat.traits$species` must contain different values", fixed = TRUE)
})


## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : mat.overlap", {
  
  ## TEST mat.overlap : correct object
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3))
               , "`mat.overlap` must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = 1)
               , "`mat.overlap` must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame())
               , "`mat.overlap` does not have the appropriate number of rows (>=2, at least 2 species) or columns (species, raster)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(1, 2))
               , "`mat.overlap` does not have the appropriate number of rows (>=2, at least 2 species) or columns (species, raster)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(c(1, 1), 2))
               , "Column names of `mat.overlap` must be `species` and `raster`", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(species = c(1, 1), 2))
               , "Column names of `mat.overlap` must be `species` and `raster`", fixed = TRUE)
  
  ## TEST mat.overlap$species : different values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(species = c(1, 1)
                                                              , raster = 2))
               , "`mat.overlap$species` must contain different values", fixed = TRUE)
  
  ## TEST mat.overlap$raster : exist
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(species = c(1, 2)
                                                              , raster = 2))
               , "`mat.overlap$raster` must contain file names which exist", fixed = TRUE)
  
  ## TEST mat.overlap$raster : correct values
  file.create("a.txt")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = data.frame(species = c(1, 2)
                                                              , raster = "a.txt"))
               , "`mat.overlap$raster` must contain file names with appropriate extension (`.tif`, `.img`, `.asc`)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : matrix
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(seq(2), ncol=2))
               , "`mat.overlap` does not have the same number of rows (1) and columns (2)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(seq(2), ncol=1))
               , "`mat.overlap` does not have the same number of rows (2) and columns (1)"
               , fixed = TRUE)
  
})


## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : rules.speciesDistance", {
  
  ## TEST opt.maxPercent.NA : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = "a"
                                                               , "opt.maxPercent.similarSpecies" = 1
                                                               , "opt.min.sd" = 1))
               , "`rules.speciesDistance['opt.maxPercent.NA']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = 1.5
                                                               , "opt.maxPercent.similarSpecies" = 1
                                                               , "opt.min.sd" = 1))
               , "`rules.speciesDistance['opt.maxPercent.NA']` must contain values between `0` and `1`"
               , fixed = TRUE)
  
  ## TEST opt.maxPercent.similarSpecies : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = 1
                                                               , "opt.maxPercent.similarSpecies" = "a"
                                                               , "opt.min.sd" = 1))
               , "`rules.speciesDistance['opt.maxPercent.similarSpecies']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = 1
                                                               , "opt.maxPercent.similarSpecies" = 1.5
                                                               , "opt.min.sd" = 1))
               , "`rules.speciesDistance['opt.maxPercent.similarSpecies']` must contain values between `0` and `1`"
               , fixed = TRUE)
  
  
  ## TEST opt.min.sd : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = 1
                                                               , "opt.maxPercent.similarSpecies" = 1
                                                               , "opt.min.sd" = "a"))
               , "`rules.speciesDistance['opt.min.sd']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , rules.speciesDistance = c("opt.maxPercent.NA" = 1
                                                               , "opt.maxPercent.similarSpecies" = 1
                                                               , "opt.min.sd" = 1.5))
               , "`rules.speciesDistance['opt.min.sd']` must contain values between `0` and `1`"
               , fixed = TRUE)
})



## INPUT
test_that("SAVE_FATE.step1_PFG gives error with wrong data : mat.species.DIST", {
  
  ## TEST mat.species.DIST : length > 0
  # expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
  #                                  , mat.observations = data.frame(species = "1"
  #                                                                  ,sites = "A"
  #                                                                  , abund = 5)
  #                                  , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
  #                                  , mat.overlap = matrix(1)
  #                                  , mat.species.DIST = list())
  #              , "`mat.species.DIST` must be of length > 0")
  
  ## TEST mat.species.DIST : dissimilarity object (list)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = 1)
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = "a")
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = factor("A"))
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  
  ## TEST mat.species.DIST : dissimilarity object
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = data.frame(1))
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = list(NA))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = list(1))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = list("a"))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = list(list()))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  
  ## TEST mat.species.DIST : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1:8, ncol = 4))
               , "`mat.species.DIST` does not have the same number of rows (2) and columns (4)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = list(matrix(1:9, ncol=3)
                                                             , matrix(1:8, ncol=4)))
               , "`mat.species.DIST[[2]]` does not have the same number of rows (2) and columns (4)"
               , fixed = TRUE)
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : clust.evaluation", {
  
  ## TEST clust.evaluation : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = NA)
               , "`clust.evaluation` must be a data.frame", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = list(1,NA))
               , "`clust.evaluation` must be a data.frame", fixed = TRUE)
  
  ## TEST clust.evaluation : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame())
               , "`clust.evaluation` does not have the appropriate number of rows (>0) or columns (GROUP, no.clusters, variable, value)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(1))
               , "`clust.evaluation` does not have the appropriate number of rows (>0) or columns (GROUP, no.clusters, variable, value)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(1,2))
               , "`clust.evaluation` does not have the appropriate number of rows (>0) or columns (GROUP, no.clusters, variable, value)"
               , fixed = TRUE)
  
  ## TEST clust.evaluation : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(c(1,1), 2, 3, 4))
               , "Column names of `clust.evaluation` must be `GROUP`, `no.clusters`, `variable` and `value`"
               , fixed = TRUE)
})



## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : no.clusters", {
  
  ## TEST no.clusters : missing / numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4))
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = NA)
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = NULL)
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = list())
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = data.frame(1))
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = factor(1))
               , "(missing `no.clusters` information)")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = "a")
               , "(missing `no.clusters` information)")
  
  ## TEST no.clusters : correct length
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = c(1,2))
               , "`no.clusters` must have the same length than `mat.species.DIST`")
})



## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : determ.all", {
  
  ## TEST determ.all : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = NA)
               , "`determ.all` must be a data.frame", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = list(1,NA))
               , "`determ.all` must be a data.frame", fixed = TRUE)
  
  ## TEST determ.all : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame())
               , "`determ.all` does not have the appropriate number of rows (>0) or columns (PFG, GROUP, ID.cluster, species, ID.species, DETERMINANT, (sp.mean.dist), (allSp.mean), (allSp.min), (allSp.max))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(1))
               , "`determ.all` does not have the appropriate number of rows (>0) or columns (PFG, GROUP, ID.cluster, species, ID.species, DETERMINANT, (sp.mean.dist), (allSp.mean), (allSp.min), (allSp.max))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(1,2))
               , "`determ.all` does not have the appropriate number of rows (>0) or columns (PFG, GROUP, ID.cluster, species, ID.species, DETERMINANT, (sp.mean.dist), (allSp.mean), (allSp.min), (allSp.max))"
               , fixed = TRUE)
  
  ## TEST determ.all : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(1,2,3,4,5,6))
               , "Column names of `determ.all` must be `PFG`, `GROUP`, `ID.cluster`, `species`, `ID.species`, `DETERMINANT`, `(sp.mean.dist)`, `(allSp.mean)`, `(allSp.min)` and `(allSp.max)`"
               , fixed = TRUE)
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong data : mat.traits.PFG", {
  
  ## TEST mat.traits.PFG : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = NA)
               , "`mat.traits.PFG` must be a data.frame", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = list(1,NA))
               , "`mat.traits.PFG` must be a data.frame", fixed = TRUE)
  
  ## TEST mat.traits.PFG : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = data.frame())
               , "`mat.traits.PFG` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = data.frame(1))
               , "`mat.traits.PFG` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = data.frame(c(1,1),2))
               , "`mat.traits.PFG` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  
  ## TEST mat.traits.PFG : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = data.frame(c(1,1),2,3))
               , "Column names of `mat.traits.PFG` must be `PFG`, `no.species`, `(trait1)`, `(trait2)` and `...`"
               , fixed = TRUE)
  
  ## TEST mat.traits.PFG : different values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , mat.traits = data.frame(species = c("A", "B"), GROUP = 1, 2, 3)
                                   , mat.overlap = matrix(1)
                                   , mat.species.DIST = matrix(1)
                                   , clust.evaluation = data.frame(GROUP = 1, no.clusters = 2, variable = 3, value = 4)
                                   , no.clusters = 1
                                   , determ.all = data.frame(PFG = 1, GROUP = 2, ID.cluster = 3
                                                             , species = 4, ID.species = 5, DETERMINANT = 6)
                                   , mat.traits.PFG = data.frame(PFG = c(1,1), no.species = 2, trait1 = 3))
               , "`mat.traits.PFG$PFG` must contain different values"
               , fixed = TRUE)
})


