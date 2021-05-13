# Constant string literals
error_X_type_string <- "[ERROR] - Parameter X is not a matrix"
error_X_hat_type_string <- "[ERROR] - Parameter X_hat is not a matrix"
error_dimension_mismatch_string <- "[ERROR] - Dimension mismatch between X and X_hat"

#' input_check
#'
#' Verifies that the input values to the error measure functions have the correct type and shapes
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return - NULL
input_check <- function(X,X_hat){
  if(!is.matrix(X)){
    stop(error_X_type_string)
  }

  if(!is.matrix(X_hat)){
    stop(error_X_hat_type_string)
  }

  if(!identical(dim(X),dim(X_hat))){
    stop(error_dimension_mismatch_string)
  }
}
