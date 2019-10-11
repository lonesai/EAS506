#####################################################################################
## Pre_mining examples
## Author: Sai Lone
## Created: 10/05
## Edited:  10/05
#####################################################################################

rm(list = ls())

# set working director
setwd("/Users/sailone/Desktop/fall19/EAS506/HW2")

#install.packages("ISLR")
#install.packages("glmnet")
#install.packages("pls")
#install.packages("leaps")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("class")



#####################################################
######### 1)  Exercise 9 modified ISL
#####################################################
# get info on the on College data set in ISLR package
library(ISLR)
data(College)
college <- College

dim(college) # Observe the numbers of rows and columns
names(college)

# a) Split the data set into a training set and a test set. 
# Fit a linear model using least squares on the training set, 
# and report the test error obtained.
#####################################################

set.seed(1) # set a random seed to make the partition reproducible

train_index <- sample(1:nrow(college), nrow(college)/2) # split the data 50/50

# Splice the data set into a training set and a test set.
train <- college[train_index,]
test <- college[(-train_index),]

# Fit a linear model using least squares on Apps
linear_mod.fit <- lm(Apps~. , data=train)
summary(linear_mod.fit)
linear_mod.pred <- predict(linear_mod.fit, test)
# Obtained test error
linear_mod.mse <- mean((test$Apps - linear_mod.pred)^2)
linear_mod.mse

# b) Fit a ridge regression model on the training set, with λ 
# chosen by cross- validation. Report the test error obtained.
#####################################################
library(glmnet) # load lib to use glmnet function
?cv.glmnet
# convert the train and test dataframe to matrix
train.matrix <- model.matrix(Apps~., data=train)
test.matrix <- model.matrix(Apps~., data=test)

# get grid as the lambda value in the ridge regression from 10^4 to 10^-2
grid <- 10^seq(4, -2, length=100)
ridge.fit <- cv.glmnet(train.matrix, train$Apps, alpha=0, lambda=grid, thresh=1e-12)
summary(ridge.fit)
lambda_best <- ridge.fit$lambda.min
lambda_best # λ chosen by crossvalidation.
ridge.pred <- predict(ridge.fit, s=lambda_best, newx=test.matrix)
ridge.mse <- mean((test$Apps - ridge.pred)^2)
ridge.mse # test error for ridge regression

# d) Fit a lasso model on the training set, with λ chosen by crossvalidation.
# Report the test error obtained, along with the number of non-zero coefficient estimates.
#####################################################

# if alpha = 1 then a lasso model is fit
lasso.fit = cv.glmnet(train.matrix, train$Apps, alpha=1, lambda=grid, thresh=1e-12)
summary(lasso.fit)

lambda_best <- lasso.fit$lambda.min
lambda_best # λ chosen by crossvalidation.
lasso.pred <- predict(lasso.fit, s=lambda_best, newx=test.matrix)
lasso.mse <- mean((test$Apps - lasso.pred)^2)
lasso.mse # test error for lasso regression

college.matrix <- model.matrix(Apps~., data=college)
lasso.fit_non_zero <- glmnet(college.matrix, college$Apps, alpha=1)
summary(lasso.fit_non_zero)
lasso.pred_non_zero <- predict(lasso.fit, s= lambda_best, type="coefficients")
lasso.pred_non_zero # non-zero coefficient estimates

# (e) Fit a PCR model on the training set, with k chosen by cross-validation. 
# Report the test error obtained, along with the value of k selected by cross-validation.
#####################################################
library(pls) # load lib to use pcr function

pcr.fit = pcr(Apps~., data=train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP") # value of k selected by cross- validation.
pcr.pred <- predict(pcr.fit,test, ncomp=10)
pcr.mse <- mean((test$Apps - pcr.pred)^2)
pcr.mse# test error for PCR model

# f) Fit a PLS model on the training set, with k chosen by crossvalidation. Report
# the test error obtained, along with the value of k selected by cross- validation.
#####################################################

psl.fit = plsr(Apps~., data=train, scale=TRUE, validation="CV")
summary(psl.fit)
validationplot(psl.fit, val.type="MSEP") # value of k selected by cross- validation.
psl.pred <- predict(psl.fit,test, ncomp=10)
psl.mse <- mean((test$Apps - psl.pred)^2)
psl.mse # test error for PCR model


#####################################################
######### 2)  tic data
#####################################################

set.seed(2) # set a random seed to make the partition reproducible

# read the txt file for training and testing data
tic_train <- read.table("ticdata2000.txt")
tic_test <- read.table("ticeval2000.txt")
tic_target <- read.table("tictgts2000.txt") # target data for test data
tic_test$V86 <- tic_target$V1 # adding the missing target column to test data


# Compute the ordinary least squares estimates
ols.fit <- lm(V86~., data=tic_train)
summary(ols.fit)
ols.pred <- predict(ols.fit, tic_test) # least squares estimates
# test error
ols.mse <- mean((tic_test$V86 - ols.pred)^2)
ols.mse # [1] 0.053985

# Compute the forwards and backwars selection estimates
library(leaps) # load lib to use regsubset funtion

regfit.fwd = regsubsets(V86~., data=tic_train, nvmax=85, method="forward")
summary(regfit.fwd)

# compute the forward model estimates
tic_test.mat = model.matrix(V86~., data=tic_test)
fwd.errors=rep(NA,85)
for(i in 1:85){
	fwd.coefi=coef(regfit.fwd,id=i)
	fwd.pred=tic_test.mat[,names(fwd.coefi)]%*%fwd.coefi
	fwd.errors[i]=mean((tic_test$V86 - fwd.pred)^2)
}

which.min(fwd.errors) # [1] 27

# lets look at the coefficient for the smallest error value
coef(regfit.fwd, 27) 
fwd.mse <- min(fwd.errors) 
# test error
fwd.mse # [1] 0.05385551
fwd.pred # estimates

# compute the backward model estimates
regfit.bwd <- regsubsets(V86~., data=tic_train, nvmax=85, method="backward")
summary(regfit.bwd)

bwd.errors=rep(NA,85)
for(i in 1:85){
	bwd.coefi=coef(regfit.bwd,id=i)
	bwd.pred=tic_test.mat[,names(bwd.coefi)]%*%bwd.coefi
	bwd.errors[i]=mean((tic_test$V86 - bwd.pred)^2)
}
bwd.mse <- min(bwd.errors) 
# test error
bwd.mse # [1] 0.05383966

bwd.pred # estimates

# Compute Lasso regression estimates
tic_train.mat <- model.matrix(V86~., data=tic_train)
lasso_tic.fit <- cv.glmnet(tic_train.mat, tic_train$V86, alpha=1)
summary(lasso_tic.fit)
bestlam <- lasso_tic.fit$lambda.min
bestlam
# Lasso regression estimates
lasso_tic.pred <- predict(lasso_tic.fit, s=bestlam, newx=tic_test.mat)
lasso_tic.pred # estimates
lasso_tic.mse <- mean((tic_test$V86 - lasso_tic.pred)^2)
lasso_tic.mse # [1] 0.05376028

# Compute Ridge regression estimates
ridge_tic.fit <- cv.glmnet(tic_train.mat, tic_train$V86, alpha=0)
summary(ridge_tic.fit)
bestlam2 <- ridge_tic.fit$lambda.min
bestlam2
# Ridge regression estimates
ridge_tic.pred <- predict(ridge_tic.fit, s=bestlam2, newx=tic_test.mat)
ridge_tic.pred # estimates
ridge_tic.mse <- mean((tic_test$V86 - ridge_tic.pred)^2)
ridge_tic.mse # [1] 0.05369629


# Plot the findings
all_results <- as.data.frame(cbind(ols.pred,fwd.pred,bwd.pred,lasso_tic.pred,ridge_tic.pred))
names(all_results) <-c('ols','fwd', 'bwd', 'lasso', 'ridge')
boxplot(all_results)


#####################################################
######### 3)  KNN on Iris data
#####################################################

library(ggplot2)
library(GGally)
library(class)
data(iris) # load the data
?iris
names(iris) 
dim(iris)

# a) Perform k-nearest neighbor on the data for a range of k values. 
# Plot the error rate as a function of k. Report the confusion matrix. 
# Comment on the ability of kNN to discriminate the various species.
#####################################################

train_index <- sample(1:nrow(iris), nrow(iris)*0.7)

# Splice the data set into a training set and a test set.
iris_train <- iris[train_index,]
iris_test <- iris[-train_index,]
dim(train)
head(train)

# save then remove the lable (variable we are predicting on) on the test data
test_lable <- iris_test$Species
iris_test <- iris_test[,1:4]
head(iris_test)

require(class) # load class lib to use the knn function

# try differnt k value to find the most optimum 
error_on_k <- c() # list to keep remember the errors on the k values we try
for (i in 1:10){
	knn_iris.fit <- knn(iris_train[,1:4], iris_test, iris_train$Species, k=i)
	error_on_k[i] = 1 - mean(knn_iris.fit == test_lable)
}

error_on_k # observe 

# plot the error rate as a function of k.
g <- ggplot(data = data.frame(error_on_k), aes(x = 1:10, y = error_on_k)) + theme(legend.position = "none") + labs(title = "error rate as a function of k")+ geom_line(color = "Blue")

# Confusion Matrix
knn_iris.pred <- knn(iris_train[,1:4], iris_test, iris_train$Species, k=1)
table(test_lable, knn_iris.pred)

# b) Perform k-nearest neighbor on the first two principal components. 
# Plot the error rate as a function of k. Report the confusion matrix. 
# Plot the scores for the first two principal components and color the 
# samples by class (Species). How does error rate compare to Part (A)?
#####################################################

test_lable <- iris_test$Species
iris_test <- iris_test[,1:4]
head(iris_test)

require(class) # load class lib to use the knn function

# try differnt k value to find the most optimum 
error_on_k <- c() # list to keep remember the errors on the k values we try
for (i in 1:10){
	knn_iris.fit <- knn(iris_train[,1:4], iris_test, iris_train$Species, k=i)
	error_on_k[i] = 1 - mean(knn_iris.fit == test_lable)
}

error_on_k # observe 

# plot the error rate as a function of k.
g <- ggplot(data = data.frame(error_on_k), aes(x = 1:10, y = error_on_k)) + theme(legend.position = "none") + labs(title = "error rate as a function of k")+ geom_line(color = "Blue")

# Confusion Matrix
knn_iris.pred <- knn(iris_train[,1:4], iris_test, iris_train$Species, k=1)
table(test_lable, knn_iris.pred)










