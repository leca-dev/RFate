library(RFate)
context("PRE_FATE.speciesDistance() function")

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with missing data", {
  expect_error(PRE_FATE.speciesDistance()
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(NA)
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(NULL)
               , "`mat.traits` must be a data.frame", fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : mat.traits", {
  
  ## TEST mat.traits : data.frame
  expect_error(PRE_FATE.speciesDistance(mat.traits = NA)
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = list(1,NA))
               , "`mat.traits` must be a data.frame", fixed = TRUE)
  
  ## TEST mat.traits : correct number of rows and columns
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame())
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(1))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(1,2))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(1, 2, 3))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 2 traits)"
               , fixed = TRUE)
  
  ## TEST mat.traits : correct names of columns
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(c(1,1), 2, 3))
               , "Column names of `mat.traits` must be `species`, `(GROUP)`, `(trait1)`, `(trait2)` and `...`"
               , fixed = TRUE)
  
  
  ## TEST mat.traits$species : different values
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c(1,1), 2, 3))
               , "`mat.traits$species` must contain different values", fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : mat.overlap", {
  
  ## TEST mat.overlap : correct object
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3))
               , "`mat.overlap` must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = 1)
               , "`mat.overlap` must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : correct number of rows and columns
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame())
               , "`mat.overlap` does not have the appropriate number of rows (>=2, at least 2 species) or columns (species, raster)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(1, 2))
               , "`mat.overlap` does not have the appropriate number of rows (>=2, at least 2 species) or columns (species, raster)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : correct names of columns
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(c(1, 1), 2))
               , "Column names of `mat.overlap` must be `species` and `raster`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(species = c(1, 1), 2))
               , "Column names of `mat.overlap` must be `species` and `raster`", fixed = TRUE)
  
  ## TEST mat.overlap$species : different values
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(species = c(1, 1)
                                                                   , raster = 2))
               , "`mat.overlap$species` must contain different values", fixed = TRUE)
  
  ## TEST mat.overlap$raster : exist
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(species = c(1, 2)
                                                                   , raster = 2))
               , "`mat.overlap$raster` must contain file names which exist", fixed = TRUE)
  
  ## TEST mat.overlap$raster : correct values
  file.create("a.txt")
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = data.frame(species = c(1, 2)
                                                                   , raster = "a.txt"))
               , "`mat.overlap$raster` must contain file names with appropriate extension (`.tif`, `.img`, `.asc`)"
               , fixed = TRUE)
  
  ## TEST mat.overlap : matrix
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(seq(2), ncol=2))
               , "`mat.overlap` does not have the same number of rows (1) and columns (2)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(seq(2), ncol=1))
               , "`mat.overlap` does not have the same number of rows (2) and columns (1)"
               , fixed = TRUE)
  
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.maxPercent.NA", {
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.NA = "a")
               , "`opt.maxPercent.NA` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.NA = factor(1))
               , "`opt.maxPercent.NA` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.NA = 1.1)
               , "`opt.maxPercent.NA` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.NA = -1.1)
               , "`opt.maxPercent.NA` must contain values between `0` and `1`", fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.maxPercent.similarSpecies", {
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.similarSpecies = "a")
               , "`opt.maxPercent.similarSpecies` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.similarSpecies = factor(1))
               , "`opt.maxPercent.similarSpecies` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.similarSpecies = 1.1)
               , "`opt.maxPercent.similarSpecies` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.maxPercent.similarSpecies = -1.1)
               , "`opt.maxPercent.similarSpecies` must contain values between `0` and `1`", fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.min.sd", {
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.min.sd = "a")
               , "`opt.min.sd` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.min.sd = factor(1))
               , "`opt.min.sd` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.min.sd = 1.1)
               , "`opt.min.sd` must contain values between `0` and `1`", fixed = TRUE)
  expect_error(PRE_FATE.speciesDistance(mat.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.overlap = matrix(1)
                                        , opt.min.sd = -1.1)
               , "`opt.min.sd` must contain values between `0` and `1`", fixed = TRUE)
})



## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output", {
  mat.traits = data.frame(species = c("a", "b", "c")
                          , TRAIT_1 = 1:3
                          , TRAIT_2 = 5:7
                          , GROUP = c("A", "A", "A"))
  mat.overlap = matrix(runif(9)
                       , ncol = 3
                       , dimnames = list(c("a", "b", "c")
                                         , c("a", "b", "c")))
  
  
  tmp1 = PRE_FATE.speciesDistance(mat.traits = mat.traits
                                  , mat.overlap = mat.overlap
                                  , opt.maxPercent.NA = 1
                                  , opt.maxPercent.similarSpecies = 0.25
                                  , opt.min.sd = 0.3)
  tmp2 = PRE_FATE.speciesDistance(mat.traits = mat.traits
                                  , mat.overlap = as.dist(mat.overlap)
                                  , opt.maxPercent.NA = 1
                                  , opt.maxPercent.similarSpecies = 0.25
                                  , opt.min.sd = 0.3)
  expect_output(str(tmp1), "dist")
  expect_equal(ncol(as.matrix(tmp1)), 3)
  expect_output(str(tmp2), "dist")
  expect_equal(ncol(as.matrix(tmp2)), 3)
  
  
  
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = 1:5
                          , TRAIT_2 = 5:9
                          , GROUP = c("A", "A", "A", "B", "B"))
  mat.overlap = matrix(runif(25)
                       , ncol = 5
                       , dimnames = list(c("a", "b", "c", "d", "e")
                                         , c("a", "b", "c", "d", "e")))
  
  tmp3 = PRE_FATE.speciesDistance(mat.traits = mat.traits
                                  , mat.overlap = mat.overlap
                                  , opt.maxPercent.NA = 1
                                  , opt.maxPercent.similarSpecies = 0.25
                                  , opt.min.sd = 0.3)
  
  expect_output(str(tmp3), "List")
  expect_equal(length(tmp3), 2)
  expect_equal(ncol(as.matrix(tmp3[[1]])), 3)
  expect_equal(ncol(as.matrix(tmp3[[2]])), 2)
  
})


## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output : warning", {
  mat.overlap = matrix(runif(25)
                       , ncol = 5
                       , dimnames = list(c("a", "b", "c", "d", "e")
                                         , c("a", "b", "c", "d", "e")))
  
  ## TEST no GROUP information provide
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = 1:5
                          , TRAIT_2 = 5:9)
  expect_warning(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                          , mat.overlap = mat.overlap)
                 , "Data will be considered as one unique dataset.")
  
  ## TEST species with no trait values
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = c(NA, 2:5)
                          , TRAIT_2 = c(NA, 6:9))
  expect_warning(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                          , mat.overlap = mat.overlap)
                 , "`mat.traits` contains some species with no trait values : ")
  
  ## TEST too many NA
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = NA
                          , TRAIT_2 = NA)
  expect_error(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                        , mat.overlap = mat.overlap)
               , "`mat.traits` does not have the appropriate number of rows (>=2)"
               , fixed = TRUE)
  
  ## TEST GROUP with only 1 species
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = 1:5
                          , TRAIT_2 = 5:9
                          , GROUP = c("A", rep("B", 4)))
  expect_warning(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                          , mat.overlap = mat.overlap)
                 , "`mat.traits` contains some groups with only one species : ")
  
  ## TEST traits with too many NA
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = 1:5
                          , TRAIT_2 = c(NA, 6, NA, NA, 9)
                          , TRAIT_3 = 10:14)
  expect_warning(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                          , mat.overlap = mat.overlap
                                          , opt.maxPercent.NA = 0.2)
                 , "`mat.traits` contains some traits with too many missing values or not enough variation between species."
                 , fixed = TRUE)
  
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = rep(1, 5)
                          , TRAIT_2 = c(NA, 6, NA, NA, 9)
                          , TRAIT_3 = c(NA, NA, 12, NA, 9))
  expect_error(PRE_FATE.speciesDistance(mat.traits = mat.traits
                                        , mat.overlap = mat.overlap
                                        , opt.maxPercent.NA = 0.2)
               , "`mat.traits` contains traits with too many missing values or not enough variation between species."
               , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output : with raster", {
  mat.traits = data.frame(species = c("a", "b", "c", "d", "e")
                          , TRAIT_1 = 1:5
                          , TRAIT_2 = 5:9
                          , GROUP = c("A", "A", "A", "B", "B"))
  
  library(raster)
  map_0 = raster(nrows = 5, ncols = 5)
  for (sp in mat.traits$species)
  {
    map_sp = map_0
    map_sp[] = runif(25)
    writeRaster(map_sp, filename = paste0("map_", sp, ".tif"), overwrite = TRUE)
  }
  
  mat.overlap = data.frame(species = mat.traits$species
                           , raster = paste0("map_", mat.traits$species, ".tif"))
  
  
  tmp1 = PRE_FATE.speciesDistance(mat.traits = mat.traits
                                  , mat.overlap = mat.overlap
                                  , opt.maxPercent.NA = 1
                                  , opt.maxPercent.similarSpecies = 0.25
                                  , opt.min.sd = 0.3)
  expect_output(str(tmp1), "List")
  expect_equal(length(tmp1), 2)
  expect_equal(ncol(as.matrix(tmp1[[1]])), 3)
  expect_equal(ncol(as.matrix(tmp1[[2]])), 2)
})

