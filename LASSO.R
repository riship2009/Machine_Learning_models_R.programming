install.packages("dslabs")
library(dslabs)
library(glmnet)
divorce_margarine
View(divorce_margarine) 

x = as.matrix(divorce_margarine)
y = divorce_margarine[,2]   # taking column that needs to be predicted

n=10
set.seed(10)
train_rows = sample(1:n, 0.6*n)  # randomly generating 1 to 10 and selecting 6 values out of them


x.train=x[train_rows,]
x.test=x[-train_rows,]
y.train=y[train_rows]
y.test=y[-train_rows]

best_lambda = cv.glmnet(x.train, y.train, type.measure = "mse", alpha=1, family="gaussian")  # "alpha = 1" states we are conducting LASSO
best_lambda   # to find best lambda value
plot(best_lambda)

predicted = predict(best_lambda, s=best_lambda$lambda.1se,newx=x.test)
predicted   # predicted "divorce_rate_maine" values 

mean((y.test-predicted)^2)   # calculating MSE

