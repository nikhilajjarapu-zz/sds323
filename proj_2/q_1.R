library(mosaic)
library(tidyverse)
library(FNN)


sclass = read.csv('./data/sclass.csv')

#define RMSE - from slides
rmse = function(y, ypred) {
  sqrt(mean((y-ypred)^2))
}

# The variables involved
summary(sclass)

# Focus on 2 trim levels: 350 and 65 AMG
sclass350 = subset(sclass, trim == '350')
dim(sclass350)

sclass65AMG = subset(sclass, trim == '65 AMG')
summary(sclass65AMG)

# Look at price vs mileage for each trim level
plot(price ~ mileage, data = sclass350)
plot(price ~ mileage, data = sclass65AMG)

#split data: x is mileage, y is price
#limit should be 75% training, 25% test
limit_350 = floor((nrow(sclass350)/4)*3)
limit_65amg = floor((nrow(sclass65AMG)/4)*3)

train_350 = sclass350[1:limit_350, ]
Xtrain_350 = train_350['mileage']
ytrain_350 = train_350$price
test_350 = sclass350[(limit_350 + 1):nrow(sclass350), ]
Xtest_350 = test_350['mileage']
ytest_350 = test_350$price

train_65amg = sclass65AMG[1:limit_65amg, ]
Xtrain_65amg = train_65amg['mileage']
ytrain_65amg = train_65amg$price
test_65amg = sclass65AMG[(limit_65amg + 1):nrow(sclass65AMG) ,]
Xtest_65amg = test_65amg['mileage']
ytest_65amg = test_65amg$price

#run k-nearest neighbors on trim 350
max_k = nrow(Xtrain_350)
error = matrix(0, max_k - 1, 1)
k_vals = 2:max_k
for (k_val in k_vals)
{
  pred_model_k = FNN::knn.reg(train = Xtrain_350, test = Xtest_350, y = ytrain_350, k = k_val)
  ypred_k = pred_model_k$pred
  rmse_out = rmse(ytest_350,ypred_k)
  error[k_val - 1] = rmse_out
}

#create RMSE vs K plot for trim 350
plot_mat = data.frame(k_vals, error)
ggplot(data = plot_mat) + 
  geom_point(mapping=aes(x = k_vals, y = error)) +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("Determining Optimal K value - Trim 350") + 
  scale_x_reverse() +
  geom_hline(yintercept = min(error), linetype="dashed", color = "red") +
  geom_text(aes(0,min(error),label = paste("optimal k value: ", which.min(error), " error: ", round(min(error), digits = 2)), vjust = -1, hjust = 1.75, color = "red"))

#create optimal model with red overlay for trim 350
min_k = which.min(error)
pred_model_min_k = FNN::knn.reg(train = Xtrain_350, test = Xtest_350, y = ytrain_350, k = min_k)
plot_base = ggplot(data = test_350) +
  geom_point(mapping=aes(x = mileage, y = price)) +
  xlab("Mileage") + 
  ylab("Price") + 
  ggtitle(paste("trim 350, k = ", min_k))

plot_base + geom_line(mapping=aes(x = Xtest_350$mileage, y = pred_model_min_k$pred, color="red"))


#run k-nearest neighbors on trim 65amg
max_k = nrow(Xtrain_65amg)
error = matrix(0, max_k - 1, 1)
k_vals = 2:max_k
for (k_val in k_vals)
{
  pred_model_k = FNN::knn.reg(train = Xtrain_65amg, test = Xtest_65amg, y = ytrain_65amg, k = k_val)
  ypred_k = pred_model_k$pred
  rmse_out = rmse(ytest_65amg,ypred_k)
  error[k_val - 1] = rmse_out
}

#create RMSE vs K plot for trim 350
plot_mat = data.frame(k_vals, error)
ggplot(data = plot_mat) + 
  geom_point(mapping=aes(x = k_vals, y = error)) +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("Determining Optimal K value - Trim 65 AMG") + 
  scale_x_reverse() +
  geom_hline(yintercept = min(error), linetype="dashed", color = "red") +
  geom_text(aes(0,min(error),label = paste("optimal k value: ", which.min(error), " error: ", round(min(error), digits = 2)), vjust = -1, hjust = 1.75, color = "red"))

#create optimal model with red overlay for trim 65amg
min_k = which.min(error)
pred_model_min_k = FNN::knn.reg(train = Xtrain_65amg, test = Xtest_65amg, y = ytrain_65amg, k = min_k)
plot_base = ggplot(data = test_65amg) +
  geom_point(mapping=aes(x = mileage, y = price)) +
  xlab("Mileage") + 
  ylab("Price") + 
  ggtitle(paste("trim 65 AMG, k = ", min_k))

plot_base + geom_line(mapping=aes(x = Xtest_65amg$mileage, y = pred_model_min_k$pred, color="red"))