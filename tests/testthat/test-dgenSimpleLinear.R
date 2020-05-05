test_that("dgenSimpleLinear() generates a data frame with continuous outcome, binary treatment indicator, and num.obs total observations", {

  num <- 100
  df <- dgenSimpleLinear(num.obs = num, p=0.3, q=0.9, seed=1)

  # Make sure intercept, treatment effect, and seed are numeric inputs only.

  expect_equal(dim(df)[1], num) # Make sure there are num.obs = num total observations in the output dataframe.
  expect_equal(dim(df)[2], 2) # Make sure there are 2 columns in the output dataframe.

})
