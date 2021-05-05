context("Multivariate Error Measure - Numeric input")

#Set up
X <- t(matrix(AirPassengers,ncol = 12))
X_hat <- X + 1
n_row <- dim(X)[1]
n_col <- dim(X)[2]
expected_length_scalar <- 1


test_that("MMSE - Numeric input", {
  results_list <- MMSE(X,X_hat)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)
})

test_that("MMAE - Numeric input", {
  results_list <- MMAE(X,X_hat)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

})

test_that("MMAPE - Numeric input", {
  results_list <- MMAPE(X,X_hat)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

})

test_that("MMASE - Numeric input", {
  results_list <- MMASE(X,X_hat)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPE
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

})

test_that("MWAPE - Numeric input", {
  results_list <- MWAPE(X,X_hat)

  # For individual WAPEs vector
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPEs vector
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

  # For the value computed according to the reference article
  expect_true(is.numeric(results_list$article_ref))
  expect_equal(length(results_list$article_ref),expected_length_scalar)
})

test_that("MMAPE_article - Numeric input", {
  results_list <- MMAPE_article(X,X_hat)

  # For individual WAPEs vector
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPEs vector
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

  # For the value computed according to the reference article
  expect_true(is.numeric(results_list$article_ref))
  expect_equal(length(results_list$article_ref),expected_length_scalar)
})

test_that("MSMAPE - Numeric input", {
  results_list <- MSMAPE(X,X_hat)

  # For individual WAPEs vector
  expect_true(is.numeric(results_list$values_vec))
  expect_equal(length(results_list$values_vec),n_col)

  # For the mean of the individual WAPEs vector
  expect_true(is.numeric(results_list$mean))
  expect_equal(length(results_list$mean),expected_length_scalar)

  # For the value computed according to the reference article
  expect_true(is.numeric(results_list$article_ref))
  expect_equal(length(results_list$article_ref),expected_length_scalar)
})
