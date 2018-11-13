###------- Starting a machine learning project ---------###

# Packages
library("tidyverse")
library(rpart)  # for regression trees
library(randomForest) # for random forests
library("modelr")  #Measuring model accuracy

#Read the data from a table and visualize
data<-read.csv(".../train.csv")

# Overview
summary(data)
names(data)
nrow(data)
ncol(data)

###------- ( Regression Tree )---------###
fit<- rpart(SalePrice~LotArea+YearBuilt, data=data)
plot(fit, uniform=TRUE)
text(fit, cex=.6)

#Predictions based on model
print("Predictions for the following 5 houses")
print(head(data))

print("The predictions are")

print(predict(fit, head(data)))
print("Actual price")
print(head(data$SalePrice))

#Getting the mean average error (MAE). On average, our predictions are off by about X.
mae(model=fit, data=data)

# Separating data into testing an training data. 
splitdata<-resample_partition(data, c(test=0.3, train=0.7))

# How many cases are in test and training set?
lapply(splitdata, dim)

# Fitting a new model to our trainig data
fit2<- rpart(SalePrice~LotArea+YearBuilt, data=splitdata$train)
mae(model=fit2, data=splitdata$test)

###------- ( Random forest )---------###

fitRandomForest <- randomForest(SalePrice~LotArea+YearBuilt, data=splitdata$train)
mae(model=fitRandomForest, data=splitdata$train)

# Competition between models adding more variables

fit2<- rpart(SalePrice~LotArea+YearBuilt+HouseStyle+BedroomAbvGr+GarageCars, data=splitdata$train)
mae(model=fit2, data=splitdata$train)

fitRandomForest <- randomForest(SalePrice~LotArea+YearBuilt+HouseStyle+BedroomAbvGr+GarageCars, data=splitdata$train)
mae(model=fitRandomForest, data=splitdata$train)

# Predict values in test set
pred <- predict(fitRandomForest, newdata=splitdata$test)

head(pred)
