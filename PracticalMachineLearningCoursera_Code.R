# load packages and data
library(caret)
library(kernlab)
data(spam)

# create training set indexes with 75% of data
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
# subset spam data to training
training <- spam[inTrain,]
# subset spam data (the rest) to test
testing <- spam[-inTrain,]
# dimension of original and training dataset
rbind("original dataset" = dim(spam),"training set" = dim(training))


# create 10 folds for cross validation and return the training set indices
folds <- createFolds(y=spam$type,k=10,list=FALSE,returnTrain=FALSE)
# structure of the training set indices
table(folds)


# load relevant libraries
library(ISLR); library(ggplot2);
# load wage data
data(Wage)
# create training and test sets
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# plot relationships between the predictors and outcome
featurePlot(x=training[,c("age","education","jobclass")], y = training$wage,plot="pairs")
