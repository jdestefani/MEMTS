context("Input verification")

X <- matrix(AirPassengers,ncol = 12)
X_hat <- X + 1
n_row <- dim(X)[1]
n_col <- dim(X)[2]
expected_length_scalar <- 1

test_that("input_check - X non numeric", {
  X <- AirPassengers[1:10]
  expect_error(input_check(X,X_hat),
               error_X_type_string,
               fixed=T)
})

test_that("input_check - X_hat non numeric", {
  X_hat <- AirPassengers[1:10]
  expect_error(input_check(X,X_hat),
               error_X_hat_type_string,
               fixed=T)
})

test_that("input_check - Dimension mismatch", {
  X_hat <- rbind(X_hat,X_hat[1,])
  expect_error(input_check(X,X_hat),
               error_dimension_mismatch_string,
               fixed=T)
})
