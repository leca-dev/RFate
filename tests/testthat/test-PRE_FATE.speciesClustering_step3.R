library(RFate)
context("PRE_FATE.speciesClustering_step3() function")


## INPUTS
test_that("PRE_FATE.speciesClustering_step3 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step3()
               , "`mat.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(NA)
               , "`mat.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(NULL)
               , "`mat.traits` must be a data.frame")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step3 gives error with wrong data : mat.traits", {
  
  ## TEST mat.traits : data.frame
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = 1)
               , "`mat.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = factor(1))
               , "`mat.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = matrix(1))
               , "`mat.traits` must be a data.frame")
  
  ## TEST mat.traits : correct number of rows and columns
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame())
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(1))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(1,2,3))
               , "`mat.traits` does not have the appropriate number of rows (>=2, at least 2 species) or columns (>=3, at least 1 trait)"
               , fixed = TRUE)
  
  ## TEST mat.traits : correct names of columns
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(c(1,1),2,3))
               , "Column names of `mat.traits` must be `species`, `PFG`, `(trait1)`, `(trait2)` and `...`"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1,1),2,3))
               , "Column names of `mat.traits` must be `species`, `PFG`, `(trait1)`, `(trait2)` and `...`"
               , fixed = TRUE)
  
  ## TEST mat.traits$species : different values
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1,1), PFG = 2, 3))
               , "`mat.traits$species` must contain different values"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1,NA), PFG = 2, 3))
               , "`mat.traits$species` must contain different values"
               , fixed = TRUE)
  
  ## TEST mat.traits$species : length > 0
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1,""), PFG = 2, 3))
               , "`mat.traits$species` must contain a character value of length > 0"
               , fixed = TRUE)
  
  
  ## TEST mat.traits$PFG : no NA values
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1, 2)
                                                                        , PFG = c(2, NA)
                                                                        , trait = 3))
               , "`mat.traits$PFG` must not contain NA values", fixed = TRUE)
  
  ## TEST mat.traits$PFG : length > 0
  expect_error(PRE_FATE.speciesClustering_step3(mat.traits = data.frame(species = c(1, 2)
                                                                        , PFG = c(2, "")
                                                                        , trait = 3))
               , "`mat.traits$PFG` must contain a character value of length > 0"
               , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.speciesClustering_step3 gives correct output", {
  
  ## TEST classic input
  mat.traits = data.frame(species = c(1,2)
                          , PFG = "A"
                          , height = 10)
  tmp1 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp1), "List")
  expect_equal(length(tmp1), 2)
  expect_output(str(tmp1$tab), "3 variables")
  expect_equal(length(tmp1$plot), 1)
  
  
  ## TEST maturity & longevity
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = "A"
                          , maturity = c(5, NA, 10)
                          , longevity = c(10, 14, NA))
  tmp2 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp2), "List")
  expect_output(str(tmp2$tab), "4 variables")
  expect_equal(length(tmp2$plot), 1)
  expect_equal(tmp2$tab$maturity, 8)
  expect_equal(tmp2$tab$longevity, 12)
  
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = c("A", "B", "B")
                          , maturity = c(5, NA, NA)
                          , longevity = c(10, 14, NA))
  tmp3 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp3), "List")
  expect_output(str(tmp3$tab), "4 variables")
  expect_equal(length(tmp3$plot), 1)
  expect_equal(tmp3$tab$maturity[2], 7)
  expect_equal(tmp3$tab$longevity[2], 14)
  
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = c("A", "B", "B")
                          , maturity = c(5, NA, 10)
                          , longevity = c(10, NA, NA))
  tmp3 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp3), "List")
  expect_output(str(tmp3$tab), "4 variables")
  expect_equal(length(tmp3$plot), 1)
  expect_equal(tmp3$tab$maturity[2], 10)
  expect_equal(tmp3$tab$longevity[2], 20)
  
  
  ## TEST height & light
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = "A"
                          , height = c(5, NA, 10)
                          , light = c(10, 14, NA))
  tmp4 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp4), "List")
  expect_output(str(tmp4$tab), "4 variables")
  expect_equal(length(tmp4$plot), 1)
  
  
  ## TEST soil_contrib & tolerance
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = "A"
                          , soil_contrib = c(1, 2.5, 2)
                          , soil_tolerance = c(1, 1, 2))
  tmp5 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp5), "List")
  expect_output(str(tmp5$tab), "5 variables")
  expect_equal(length(tmp5$plot), 1)
  expect_equal(tmp5$tab$soil_contrib, 1.83)
  expect_equal(tmp5$tab$soil_tol_min, 0.5)
  expect_equal(tmp5$tab$soil_tol_max, 3.17)
  
  mat.traits = data.frame(species = c(1, 2, 3)
                          , PFG = "A"
                          , soil_contrib = c(1, 2.5, 2)
                          , soil_tol_min = c(1, 2.5, 2) - c(1, 1, 2)
                          , soil_tol_max = c(1, 2.5, 2) + c(1, 1, 2))
  tmp5 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp5), "List")
  expect_output(str(tmp5$tab), "5 variables")
  expect_equal(length(tmp5$plot), 1)
  expect_equal(tmp5$tab$soil_contrib, 1.83)
  expect_equal(tmp5$tab$soil_tol_min, 0.5)
  expect_equal(tmp5$tab$soil_tol_max, 3.17)
  
  
  ## TEST other traits
  mat.traits = data.frame(species = 1:30
                          , PFG = c(rep("A", 10), rep("B", 5), rep("C", 15))
                          , light = sample(1:5, 30, replace = TRUE)
                          , LDMC = rnorm(30)
                          , type = sample(c("H", "C", "P")
                                          , size = 30
                                          , replace = TRUE
                                          , prob = c(0.5, 0.3, 0.2))
                          , stringsAsFactors = FALSE)
  mat.traits$light = ordered(factor(mat.traits$light, 1:5))
  tmp6 = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
  expect_output(str(tmp6), "List")
  expect_output(str(tmp6$tab), "5 variables")
  expect_equal(length(tmp6$plot), 3)
  
})
