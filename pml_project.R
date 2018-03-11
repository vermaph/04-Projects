library(corrplot)
library(caret)
set.seed(123)
#Reading the dataset
testing <- read_csv("C:/Users/piyus/Desktop/pml-testing.csv")
training <- read_csv("C:/Users/piyus/Desktop/pml-training.csv")

#Dimensions of training
dim(training)

#Position of "classe" column (outcome) = 155
col_names<-names(training_red)[order(names(training_red),decreasing = FALSE)]
col_order<-order(names(training_red),decreasing = FALSE)
all_cols_pos<-data.frame(cbind(col_names,col_order))
all_cols_pos[all_cols_pos$col_names == "classe",]


#Removing first 5 columns because they are not helpful in prediction
training_red<-training[,-c(1:5)]
testing_red<-testing[,-c(1:5)]

#Summary of training
#Reveals presence of many columns with NAs
summary(training_red)

#Removing columns with NA
prop_NA<-vector()
order<-vector()
      for (i in 1:dim(training_red)[2]-1)
      {
      order[i]<-i  
      prop_NA[i]<-round(sum(is.na(training_red[,i]))/dim(training_red)[1],3)
      }
col_checks_NA<-data.frame(cbind(order,prop_NA))
x<-col_checks_NA[col_checks_NA$prop_NA>0.5,] #Columns where proportion of NAs is too much >0.5, 100 columns
training_red_final<-training_red[,-c(x[,1])]
testing_red_final<-testing_red[,-c(x[,1])]

#Checking variance among predictors, in case we need a PCA preprocessing
zero_var<-nearZeroVar(training_red_final,saveMetrics = TRUE)

#Removing new_window as it has zero variance 
#Also num_window because percentUnique is too low
training_red_final<-training_red_final[,-c(1,2)]
testing_red_final<-testing_red_final[,-c(1,2)]

#We still find that there are 3 rows where some cells are NA. Because we cant calculate correlation, 
#we will remove these observations
sum(is.na(training_red_final)) #training has 3 observations
sum(is.na(testing_red_final)) #testing has zero such observations

training_red_final2<-training_red_final[complete.cases(training_red_final),]

#5373 observation in training_red_final above had 3 NAs
which(complete.cases(training_red_final)==0)


#Checking independence of predictors
res<-cor(training_red_final2[,-53])
corrplot(res, type = "upper", order = "hclust")

#From the corrplot we can say that predictors like 
#acc_belt_z is highly -vely correlated to acc_belt_y and 2 others and hence multi-collinearity exists
#Based on manual selection, removing following columns because of their correlation with other predictors
#---accel_belt_z 10
#---accel_arm_y 22
#---yaw_belt 3
#---magnet_belt_x 11
training_red_final3<-training_red_final2[,-c(10,22,3,11)]
testing_red_final3<-testing_red_final[,-c(10,22,3,11)]

#Creating training and testing dataset within original training dataset to look at 
#out of sample error rates fro m3 different models: 
#Random forest, gradient boost model and linear discriminant analysis
train_obj<-createDataPartition(training_red_final3$classe, p = 0.6, list = FALSE)
intrain<-training_red_final3[train_obj,]
intest<-training_red_final3[-train_obj,]

system.time(mod_rf<-train(classe ~ .,data = intrain,method = "rf"))
system.time(mod_gbm<-train(classe ~ .,data = intrain,method = "gbm"))
system.time(mod_lda<-train(classe ~ .,data = intrain,method = "lda"))


pred_rf<-predict(mod_rf,intest)
confusionMatrix(pred_rf,intest$classe)
#Accuracy : 0.9895

pred_gbm<-predict(mod_gbm,intest)
confusionMatrix(pred_gbm,intest$classe)
#Accuracy : 0.9568 

pred_lda<-predict(mod_lda,intest)
confusionMatrix(pred_lda,intest$classe)
#Accuracy : 0.6761

#Since randomw forest gave me the best out of sample accuracy
#I will use the model on final testing dataset
pred_testing<-predict(mod_rf,testing_red_final3)
pred_testing

varImp(mod_rf)
#roll_belt, pitch_forearm, roll_forearm are the top 3 important predictors




