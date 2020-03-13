library(tidyverse)
library(mosaic)
library(FNN)
data(SaratogaHouses)


#rmse function
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

#80% training data, 20% test data
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#100 different random splits
rmse_vals_medvsajjarapu = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # Fit to the training data, copy over medium model
  lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
                   fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=saratoga_train)
  
  lm_ajjarapu = lm(price ~ . , data=saratoga_train)
  
  # Predictions out of sample
  yhat_test1 = predict(lm_medium, saratoga_test)
  yhat_test2 = predict(lm_ajjarapu, saratoga_test)
  
  c(rmse(saratoga_test$price, yhat_test1),
    rmse(saratoga_test$price, yhat_test2))
}

#code to compare model against medium model
means = colMeans(rmse_vals_medvsajjarapu)
print(paste("Tax Department model's average error over 100 iterations: "), means[1])
print(paste("Ajjarapu and Ajjarapu model's average error over 100 iterations: "), means[2])

#fit the model for multiple Ks
k_limit = 20
k_vals = 2:k_limit
rmse_vals_knnvsajjarapu = matrix(0, k_limit - 1 ,2)

#80% training data, 20% test data
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

for (k_val in k_vals) {
  rmse_vals_iter = do(100)*{
   
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    saratoga_train = SaratogaHouses[train_cases,]
    saratoga_test = SaratogaHouses[test_cases,]
    
    #create KNN model - no pairwise interactions
    Xtrain_temp = model.matrix(price ~ . - 1, data = saratoga_train)
    Xtest_temp = model.matrix(price ~ . - 1, data = saratoga_test)
    
    ytrain = saratoga_train$price
    ytest = saratoga_test$price
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # predictions out of sample
    c(rmse(ytest, knn_model$pred),
      means[2])
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  rmse_vals_knnvsajjarapu[k_val - 1, 1] = rmse_vals_avg[1]
  rmse_vals_knnvsajjarapu[k_val - 1, 2] = rmse_vals_avg[2]
}

#plot RMSE vs K
error = rmse_vals_knnvsajjarapu[,1]
plot_mat = data.frame(k_vals, error)
ggplot(data = plot_mat) + 
  geom_point(mapping=aes(x = k_vals, y = error)) +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("RMSE vs K") + 
  scale_x_reverse() +
  geom_hline(yintercept = min(error), linetype="dashed", color = "red") +
  geom_text(aes(0,min(error),label = paste("optimal k value: ", which.min(error), " error: ", round(min(error), digits = 2)), vjust = -1, hjust = 1.75, color = "red"))