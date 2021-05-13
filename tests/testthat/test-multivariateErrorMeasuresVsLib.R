context("Multivariate Error Measures - Vs Lib")

library(UEMTS)

#Set up - Data
X <- EuStockMarkets
splitting_point <- round(2*nrow(X)/3)
X_train <- X[1:splitting_point,]
X_pred <- X_train + matrix(rnorm(nrow(X_train)*ncol(X_train),0,0.001),dim(X_train))

test_that("[Error Measure] - Multivariate MSE", {
  MMSE_lib <- MMSE(X_train,X_pred)
  MSE_vec <- numeric()
  for (i in 1:ncol(X_train)) {
    MSE_vec <- c(UEMTS::MSE(X_train[,i],X_pred[,i]),MSE_vec)
  }
  expect_equal(MMSE_lib$mean,mean(MSE_vec))
})

test_that("[Error Measure] - Multivariate MAE", {
  MMAE_lib <- MMAE(X_train,X_pred)
  MAE_vec <- numeric()
  for (i in 1:ncol(X_train)) {
    MAE_vec <- c(UEMTS::MAE(X_train[,i],X_pred[,i]),MAE_vec)
  }
  expect_equal(MMAE_lib$mean,mean(MAE_vec))
})

test_that("[Error Measure] - Multivariate MASE", {
  MMASE_lib <- MMASE(X_train,X_pred)
  MASE_vec <- numeric()
  for (i in 1:ncol(X_train)) {
    MASE_vec <- c(UEMTS::MASE(X_train[,i],X_pred[,i]),MASE_vec)
  }
  expect_equal(MMASE_lib$mean,mean(MASE_vec))
})

test_that("[Error Measure] - Multivariate MAPE", {
  MMAPE_lib <- MMAPE(X_train,X_pred)
  MAPE_vec <- numeric()
  for (i in 1:ncol(X_train)) {
    MAPE_vec <- c(UEMTS::MAPE(X_train[,i],X_pred[,i]),MAPE_vec)
  }
  expect_equal(MMAPE_lib$mean,mean(MAPE_vec))
})

