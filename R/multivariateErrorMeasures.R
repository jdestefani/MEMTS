#' MMSE - Multivariate Mean Squared Error
#'
#' Computes the multivariate MMSE as a mean of the MSEs of the individual time series
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual MSEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual MSEs}
#'         }
#'
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MMSE(X,X_hat)
#' results_list$values_vec # For individual MSEs vector
#' results_list$mean # For the mean of the individual MSEs
MMSE <- function(X,X_hat){
  input_check(X,X_hat)
  mse_vec <- apply(X - X_hat,2,function(x){mean(x^2)})
  return(list(values_vec=mse_vec,mean=mean(mse_vec)))
}

#' MMAE - Multivariate Mean Absolute Error
#'
#' Computes the multivariate MMAE as a mean of the MAEs of the individual time series
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual MAEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual MAEs}
#'         }
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MMAE(X,X_hat)
#' results_list$values_vec # For individual MAEs vector
#' results_list$mean # For the mean of the individual MAEs
MMAE <- function(X,X_hat){
  input_check(X,X_hat)
  mae_vec <- apply(abs(X - X_hat),2,mean)
  return(list(values_vec=mae_vec,mean=mean(mae_vec)))
}

#' MMAPE - Multivariate Mean Absolute Percentage Error
#'
#' Computes the multivariate MAPE as a mean of the MAPEs of the individual time series
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual MAPEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual MAPEs}
#'         }
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MMAPE(X,X_hat)
#' results_list$values_vec # For individual MAPEs vector
#' results_list$mean # For the mean of the individual MAPEs
MMAPE <- function(X,X_hat){
  input_check(X,X_hat)
  mape_vec <- 100*apply(abs((X - X_hat)/X),2,mean)
  return(list(values_vec=mape_vec,mean=mean(mape_vec)))
}

#' MMASE - Multivariate Mean Absolute Scaled Error
#'
#' Computes the multivariate MASE as a mean of the MASEs of the individual time series
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual MASEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual MASEs}
#'         }
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MMASE(X,X_hat)
#' results_list$values_vec # For individual MASEs vector
#' results_list$mean # For the mean of the individual MASEs vector
MMASE <- function(X,X_hat){
  input_check(X,X_hat)
  X_sum_err <- apply(abs(X - X_hat),2,sum)
  X_sum_diff <- apply(X, 2,function(x){sum(abs(diff(x)))})
  length_vec <- apply(X, 2, length)
  mase_vec <- ((length_vec-1)/length_vec)*X_sum_err/X_sum_diff
  return(list(values_vec=mase_vec,mean=mean(mase_vec)))
}

#' MWAPE - Multivariate Weighted Absolute Percentage Error
#'
#' Computes the multivariate WAPE based on the individual time series (according to:https \url{https://arxiv.org/pdf/1905.03806.pdf})
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual WAPEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual WAPEs}
#'         \item{\code{article_ref}: }{WAPE computed according to the reference article}
#'         }
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MWAPE(X,X_hat)
#' results_list$values_vec # For individual WAPEs vector
#' results_list$mean # For the mean of the individual WAPEs vector
#' results_list$article_ref # For the value computed according to the reference article
MWAPE <- function(X,X_hat){
  input_check(X,X_hat)
  num_vec <- apply(abs(X-X_hat), 2, sum)
  den_vec <- apply(abs(X),2,sum)
  wape_vec <- num_vec/den_vec
  return(list(values_vec=wape_vec,
              mean=mean(wape_vec,na.rm=T),
              article_ref=sum(wape_vec,na.rm=T)))
}

#' MMAPE_article - Multivariate Mean Absolute Percentage Error (according to reference article)
#'
#' Computes the multivariate MAPE based on the individual time series (according to \url{https://arxiv.org/pdf/1905.03806.pdf})
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual MAPEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual MAPEs}
#'         \item{\code{article_ref}: }{MAPE computed according to the reference article}
#'         }
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MMAPE_article(X,X_hat)
#' results_list$values_vec # For individual WAPE
#' results_list$mean # For the mean of the individual WAPE
#' results_list$mean # For the value computed according to the reference article
MMAPE_article <- function(X,X_hat){
  input_check(X,X_hat)
  filter <- abs(X) > 0
  z0_vec <- apply(abs(X)>0, 2, sum)
  wape_results <- MWAPE(X * filter,X_hat * filter)
  return(list(values_vec=wape_results$values_vec/z0_vec,
              mean=mean(wape_results$values_vec/z0_vec,na.rm=T),
              article_ref=wape_results$article_ref/sum(z0_vec,na.rm = T)))
}

#' MSMAPE - Multivariate Symmetric Mean Absolute Percentage Error
#'
#' Computes the multivariate SMAPE based on the individual time series (according to \url{https://arxiv.org/pdf/1905.03806.pdf})
#'
#' @param X - Target matrix (hxk) for k variables and forecasting horizon h
#' @param X_hat - Predicted matrix (hxk) for k variables and forecasting horizon h
#'
#' @return List containing:
#'         \itemize{
#'         \item{\code{values_vec}: }{Vector of the individual SMAPEs}
#'         \item{\code{mean}: }{Mean of the vector of the individual SMAPEs}
#'         \item{\code{article_ref}: }{SMAPE computed according to the reference article}
#'         }
#'
#'
#' @export
#' @examples
#' X <- matrix(rnorm(100),10,10)
#' X_hat <- matrix(rnorm(100,mean=1),10,10)
#' results_list <- MSMAPE(X,X_hat)
#' results_list$values_vec # For individual WAPE
#' results_list$mean # For the mean of the individual WAPE
#' results_list$mean # For the value computed according to the reference article
MSMAPE <- function(X,X_hat){
  input_check(X,X_hat)
  filter <- abs(X) > 0
  z0_vec <- apply(abs(X)>0, 2, sum)
  num_vec <- apply(abs(X-X_hat) * filter, 2, sum)
  den_vec <- apply(abs(X+X_hat),2,sum)
  smape_vec <- (2*num_vec)/den_vec
  return(list(values_vec=smape_vec/z0_vec,
              mean=mean(smape_vec/z0_vec,na.rm=T),
              article_ref=sum(smape_vec,na.rm = T)/sum(z0_vec,na.rm = T)))
}
