# create models for data
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(cluster)
library(tidyverse)
library(FNN)
library(glmnet)

# read data
X_data_unfiltered = read.csv('./data/X_data.csv', header=FALSE)
y_data_unfiltered = read.csv('./data/y_data.csv', header=FALSE)

X_data = tail(X_data_unfiltered, -1)
y_data = tail(y_data_unfiltered, -1)

data_proj = data.frame(X_data, y_data)

names(data_proj) = c("college_xpm", "college_xpa", "college_fgm", "college_fga", "avg_diff_xp", "avg_diff_fg", "Y")


# rmse function
rmse = function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm=TRUE))
}

# variables that control how long the program takes to run
num_splits = 50 #200
k_limit = 50

# model 1: linear model
#model 1: linear regression model (RMSE)
#80% training data, 20% test data
n = nrow(data_proj)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#200 different random splits
lm_vals = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = data_proj[train_cases, 1:ncol(data_proj)]
  on_test = data_proj[test_cases, 1:ncol(data_proj)]
  
  lm_ajjarapu = lm(Y ~ ., data=on_train)
  lm_ajjarapu_2 = lm(Y ~ .^2, data=on_train)
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  y_test_2 = predict(lm_ajjarapu_2, on_test)
  
  c(rmse(y_test, on_test$Y),rmse(y_test_2, on_test$Y))
}
lm_avg = unname(colMeans(lm_vals))

#model 2: knn (RMSE)
k_vals = 2:k_limit
knn_vals = matrix(0, k_limit - 1, 2)

for (k_val in k_vals) {
  rmse_vals_iter = do(num_splits)*{
    
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    on_train = data_proj[train_cases,1:ncol(data_proj)]
    on_test = data_proj[test_cases,1:ncol(data_proj)]
    
    #create KNN model
    Xtrain_temp = model.matrix(Y ~ . - 1, data = on_train)
    Xtest_temp = model.matrix(Y ~ . - 1, data = on_test)
    
    ytrain = on_train$Y
    ytest = on_test$Y
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # Predictions out of sample + convert to binary
    yhat_test1_pred = knn_model$pred
    
    c(k_val,rmse(yhat_test1_pred, ytest))
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  knn_vals[k_val - 1,] = rmse_vals_avg
}
knn_rmse = unname(knn_vals[which.min(knn_vals[,2]),])

knn_vals_rmse <- data.frame(knn_vals)
ggplot(data=knn_vals_rmse) +
  geom_line(aes(x = X1, y = X2), color='red') +
  ggtitle("RMSE for Each Value of K") +
  xlab("K") +
  ylab("RMSE")

#model 3: lasso regression/ridge regression (RMSE)
vals_lr_rr = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = data_proj[train_cases,1:ncol(data_proj)]
  on_test = data_proj[test_cases,1:ncol(data_proj)]
  
  temp_train = model.matrix.lm(Y ~ . - 1, data = on_train, na.action=na.pass)
  temp_test = model.matrix.lm(Y ~ . - 1, data = on_test, na.action=na.pass)
  x_train = temp_train[complete.cases(temp_train),]
  y_train = on_train$Y[complete.cases(temp_train)]
  x_test = temp_test[complete.cases(temp_test),]
  y_test = on_test$Y[complete.cases(temp_test)]
  
  # lasso regression
  cv_fit_l = cv.glmnet(x_train, y_train, family="gaussian", alpha = 1)
  # ridge regression
  cv_fit_r = cv.glmnet(x_train, y_train, family="gaussian", alpha = 0)
  
  opt_lambda_l = cv_fit_l$lambda.min
  opt_lambda_r = cv_fit_r$lambda.min
  
  y_pred_l = predict(cv_fit_l$glmnet.fit, s = opt_lambda_l, newx = x_test)
  y_pred_r = predict(cv_fit_r$glmnet.fit, s = opt_lambda_r, newx = x_test)
  
  c(rmse(y_pred_l, y_test), rmse(y_pred_r, y_test))
}
lr_model_avg = min(vals_lr_rr[,1])
rr_model_avg = min(vals_lr_rr[,2])

#model 4: logistic regression
vals_logm = do(num_splits)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = data_proj[train_cases,1:ncol(data_proj)]
  on_test = data_proj[test_cases,1:ncol(data_proj)]
  
  lm_ajjarapu = glm(Y ~ ., data=on_train, family=gaussian, maxit = 100)
  lm_ajjarapu_2 = glm(Y ~ .^2, data=on_train, family=gaussian, maxit = 100)
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  y_test_2 = predict(lm_ajjarapu_2, on_test)
  
  c(rmse(y_test, on_test$Y), rmse(y_test_2, on_test$Y))
}
logm_vals = unname(colMeans(vals_logm))


# plot(on_test$Y, ty = "l")
# lines(lm(Rent ~ ., data=on_test)$fitted.values, ty = "l", col="red")
# lines(glm(Rent ~ ., data=on_test, family=gaussian, maxit = 100)$fitted.values, ty = "l", col="blue")


cat("MODEL SUCCESS:")
cat("1) LINEAR REGRESSION MODEL (without interactions) - RMSE:", lm_avg[1])
cat("1) LINEAR REGRESSION MODEL (with interactions) - RMSE:", lm_avg[2])
cat("2) KNN ( k =",knn_rmse[1],") - RMSE:", knn_rmse[2])
cat("3) LASSO REGRESSION - RMSE:", lr_model_avg[1])
cat("3) RIDGE REGRESSION - RMSE:", rr_model_avg[1])
cat("5) LOGISTIC REGRESSION (without interactions) - RMSE:", logm_vals[1])
cat("5) LOGISTIC REGRESSION (with interactions) - RMSE:", logm_vals[2])

