library(mosaic)
library(tidyverse)
library(FNN)

#read in data
online_news = read.csv('./data/online_news.csv')

#80% training data, 20% test data
n = round(nrow(online_news)/100) # nearest integer
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#method 1: regression + thresholding
#model 1: linear model

#100 different random splits
vals_lm_1 = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = online_news[train_cases,2:ncol(online_news)]
  on_test = online_news[test_cases,2:ncol(online_news)]
  
  lm_ajjarapu = lm(shares ~ ., data=on_train) 
  
  # Predictions out of sample + convert to binary
  yhat_test1 = predict(lm_ajjarapu, on_test)
  yhat_binary = ifelse(yhat_test1 > 1400, 1, 0)
  y_binary = ifelse(on_test$shares > 1400, 1, 0)
  
  #calculate confusion matrix, error rate, true positive rate, false positive rate 
  conf_matrix = table(y = y_binary, yhat = yhat_binary)
  error_rate = (conf_matrix[2] + conf_matrix[3]) / sum(conf_matrix)
  true_positive_rate = conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]) 
  false_positive_rate = conf_matrix[3] / (conf_matrix[1] + conf_matrix[3]) 
  
  c(conf_matrix, error_rate, true_positive_rate, false_positive_rate)
}
vals_lm_avg = colMeans(vals_lm_1)

print("Confusion Matrix: ")
print("      yhat")
print("y      0     1")
print(paste("  0 ", vals_lm_avg[1], "  ", vals_lm_avg[3]))
print(paste("  1 ", vals_lm_avg[2], "  ", vals_lm_avg[4]))
print(paste("\nOverall Error Rate: ", vals_lm_avg[5]))
print(paste("True Positive Rate: ", vals_lm_avg[6]))
print(paste("False Positive Rate: ", vals_lm_avg[7]))

null_error_rate = 1 - (vals_lm_avg[1] + vals_lm_avg[3]) / sum(vals_lm_avg[1:4])
print(paste("Error Rate of Null Model: ", null_error_rate))

#model 2: knn
k_limit = 20
k_vals = 2:k_limit
vals_knn_1 = matrix(0, k_limit - 1, 7)

for (k_val in k_vals) {
  rmse_vals_iter = do(100)*{
    
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    on_train = online_news[train_cases,2:ncol(online_news)]
    on_test = online_news[test_cases,2:ncol(online_news)]
    
    #create KNN model - no pairwise interactions, remove features that don't scale properly
    Xtrain_temp = model.matrix(shares ~ . - (weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend) - 1, data = on_train)
    Xtest_temp = model.matrix(shares ~ . - (weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend) - 1, data = on_test)
    
    ytrain = on_train$shares
    ytest = on_test$shares
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # Predictions out of sample + convert to binary
    yhat_test1 = knn_model$pred
    yhat_binary = ifelse(yhat_test1 > 1400, 1, 0)
    y_binary = ifelse(on_test$shares > 1400, 1, 0)
    
    #calculate confusion matrix, error rate, true positive rate, false positive rate 
    conf_matrix = table(y = y_binary, yhat = yhat_binary)
    error_rate = (conf_matrix[2] + conf_matrix[3]) / sum(conf_matrix)
    true_positive_rate = conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]) 
    false_positive_rate = conf_matrix[3] / (conf_matrix[1] + conf_matrix[3]) 
    
    c(conf_matrix, error_rate, true_positive_rate, false_positive_rate)
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  vals_knn_1[k_val - 1,] = rmse_vals_avg
}
best_model_knn = vals_knn_1[which.min(vals_knn_1[,5]),]
print("Confusion Matrix: ")
print("      yhat")
print("y      0     1")
print(paste("  0 ", best_model_knn[1], "  ", best_model_knn[3]))
print(paste("  1 ", best_model_knn[2], "  ", best_model_knn[4]))
print(paste("Overall Error Rate: ", best_model_knn[5]))
print(paste("True Positive Rate: ", best_model_knn[6]))
print(paste("False Positive Rate: ", best_model_knn[7]))

null_error_rate = (best_model_knn[1] + best_model_knn[3]) / sum(best_model_knn[1:4])
print(paste("Error Rate of Null Model: ", null_error_rate))

#method 2: classification
#model 1: logistic model
vals_lm_2 = do(100)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = online_news[train_cases,2:ncol(online_news)]
  on_test = online_news[test_cases,2:ncol(online_news)]
  
  on_train$viral = ifelse(on_train$shares > 1400, 1,
                          ifelse(on_train$shares <= 1400, 0, NA))
  on_test$viral = ifelse(on_test$shares > 1400, 1,
                          ifelse(on_test$shares <= 1400, 0, NA))
  lm_ajjarapu = glm(viral ~ ., data=on_train, family=binomial, maxit = 100) 
  
  # Predictions out of sample + convert to binary
  yhat_test1_pred = predict(lm_ajjarapu, on_test, type='response')
  yhat_binary = ifelse(yhat_test1_pred > 0.5, 1, 0)
  y_binary = on_test$viral
  
  #calculate confusion matrix, error rate, true positive rate, false positive rate 
  conf_matrix = table(y = y_binary, yhat = yhat_binary)
  error_rate = (conf_matrix[2] + conf_matrix[3]) / sum(conf_matrix)
  true_positive_rate = conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]) 
  false_positive_rate = conf_matrix[3] / (conf_matrix[1] + conf_matrix[3]) 
  
  c(conf_matrix, error_rate, true_positive_rate, false_positive_rate)
}
vals_logm_avg = colMeans(vals_lm_2)
vals_lm_avg = vals_logm_avg
print("Confusion Matrix: ")
print("      yhat")
print("y      0     1")
print(paste("  0 ", vals_lm_avg[1], "  ", vals_lm_avg[3]))
print(paste("  1 ", vals_lm_avg[2], "  ", vals_lm_avg[4]))
print(paste("\nOverall Error Rate: ", vals_lm_avg[5]))
print(paste("True Positive Rate: ", vals_lm_avg[6]))
print(paste("False Positive Rate: ", vals_lm_avg[7]))

null_error_rate = 1 - (vals_lm_avg[1] + vals_lm_avg[3]) / sum(vals_lm_avg[1:4])
print(paste("Error Rate of Null Model: ", null_error_rate))

#model 2: knn
k_limit = 20
k_vals = 2:k_limit
vals_knn_2 = matrix(0, k_limit - 1, 7)

for (k_val in k_vals) {
  rmse_vals_iter = do(100)*{
    
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    on_train = online_news[train_cases,2:ncol(online_news)]
    on_test = online_news[test_cases,2:ncol(online_news)]
    
    on_train$viral = ifelse(on_train$shares > 1400, 1,
                            ifelse(on_train$shares <= 1400, 0, NA))
    on_test$viral = ifelse(on_test$shares > 1400, 1,
                           ifelse(on_test$shares <= 1400, 0, NA))
    
    #create KNN model - no pairwise interactions, remove features that don't scale properly
    Xtrain_temp = model.matrix(shares ~ . - (weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend) - 1, data = on_train)
    Xtest_temp = model.matrix(viral ~ . - (weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday + is_weekend) - 1, data = on_test)
    
    ytrain = on_train$viral
    ytest = on_test$viral
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # Predictions out of sample + convert to binary
    yhat_test1_pred = knn_model$pred
    yhat_binary = ifelse(yhat_test1_pred > 0.5, 1, 0)
    y_binary = on_test$viral
    
    #calculate confusion matrix, error rate, true positive rate, false positive rate 
    conf_matrix = table(y = y_binary, yhat = yhat_binary)
    error_rate = (conf_matrix[2] + conf_matrix[3]) / sum(conf_matrix)
    true_positive_rate = conf_matrix[4] / (conf_matrix[2] + conf_matrix[4]) 
    false_positive_rate = conf_matrix[3] / (conf_matrix[1] + conf_matrix[3]) 
    
    c(conf_matrix, error_rate, true_positive_rate, false_positive_rate)
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  vals_knn_2[k_val - 1,] = rmse_vals_avg
}
best_model = vals_knn_2[which.min(vals_knn_2[,5]),]

vals_lm_avg = best_model
print("Confusion Matrix: ")
print("      yhat")
print("y      0     1")
print(paste("  0 ", vals_lm_avg[1], "  ", vals_lm_avg[3]))
print(paste("  1 ", vals_lm_avg[2], "  ", vals_lm_avg[4]))
print(paste("Overall Error Rate: ", vals_lm_avg[5]))
print(paste("True Positive Rate: ", vals_lm_avg[6]))
print(paste("False Positive Rate: ", vals_lm_avg[7]))

null_error_rate = (vals_lm_avg[1] + vals_lm_avg[3]) / sum(vals_lm_avg[1:4])
print(paste("Error Rate of Null Model: ", null_error_rate))

