test_that("dgenSimpleLinear() generates a data frame with continuous outcome, binary treatment indicator, and num.obs total observations", {

  # SKIP FOR NOW
  # Make sure that the appropriate errors are thrown when someone uses 
  # the incorrect type of input for the function parameters.
  # SKIP FOR NOW

  num <- 100
  df <- dgenSimpleLinear(num.obs = num, intercept=2, treatment.effect=-4, seed=1)
  Yflag <- TRUE
  for(i in 1:num){
  	if(is.numeric(df$Y[i]) == FALSE){Yflag <- FALSE}
  }
  Dflag <- TRUE
  for(i in 1:num){
  	if((df$D[i] != 0) && (df$D[i] != 1)){Dflag <- FALSE}
  }

  # Make sure there are num.obs = num total observations in the output dataframe.
  expect_equal(dim(df)[1], num)
  # Make sure there are 2 columns in the output dataframe.
  expect_equal(dim(df)[2], 2) 
  # Make sure every entry of df$Y is numeric
  expect_equal(Yflag, TRUE)
  # Make sure every entry of df$D is either a 0 or a 1
  expect_equal(Dflag, TRUE)

})
