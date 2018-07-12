# Zurich Data Case Study
# Author: Piyush Verma
# Date: 06-22-2018

##################################################### Data Reading ######################################################## 
setwd("C:/0000/04-Projects/Risk Analytics Insurance Case Study")
list.files()

library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(VIM)
library(mice)
library(corrplot)
library(randomForest)  
library(Matrix)
library(ROCR)
library(xgboost)
library(caret)
library(car)
library(cluster)  
library(Rtsne)    
library(RODBC)    
library(dplyr)
library(kableExtra)
library(Metrics)
library(GGally)


raw_data_2017 <- suppressMessages(as.data.frame(read_csv("./auto_policies_2017.csv")))
numclaims<-raw_data_2017$numclaims
claimcst0<-raw_data_2017$claimcst0
raw_data_2018 <- suppressMessages(as.data.frame(read_csv("./auto_potential_customers_2018.csv")))
quote_number<-raw_data_2018$quote_number

                  
#################################################### Data Preprocessing #################################################### 
# Missing Values
aggr_plot <- aggr(raw_data_2017, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(raw_data_2017), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# Variables sorted by number of missings: 
# Variable      Count
# claim_office  0.83391840
# agecat        0.07999404
# traffic_index 0.05800437
# credit_score  0.04638032

# Only 13% observations have all values, rest of 87% observations miss values for one, two, three or four features
# Since 83% observations are missing values for claim_office, which is more than recommended 5%, we need to remove it

# Removing Claim Office (After looking at its distribution)
ggplot(raw_data_2017, aes(x=claim_office)) + 
  geom_bar(stat="count", fill = "steelblue", col = "black") +
  ggtitle("Count by Claims Office") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("")
raw_data_2017<-raw_data_2017[,-12]

raw_data_2017$date_of_birth<-as.Date(raw_data_2017$date_of_birth,"%m/%d/%Y")
raw_data_2017$age<-as.numeric(floor((today() - raw_data_2017$date_of_birth)/365))
raw_data_2018$date_of_birth<-as.Date(raw_data_2018$date_of_birth,"%m/%d/%Y")
raw_data_2018$age<-as.numeric(floor((today() - raw_data_2018$date_of_birth)/365))

# we can shift the focus now on imputing the other two features: traffic_index & credit_score
marginplot(raw_data_2017[c("traffic_index","credit_score")])
# looking at the plot, we can say that the data for both the features are missing at random (there is no difference in the distribution of the boxplot): MCAR


# Training Dataset
raw_data_2017<-raw_data_2017 %>% 
  mutate(agecat = ifelse(is.na(agecat),
                         ifelse(18<=age & age<28,1,
                                ifelse(28<=age & age<38,2,
                                       ifelse(38<=age & age<48,3,
                                              ifelse(48<=age & age<58,4,
                                                     ifelse(58<=age & age<68,5,6))))),agecat))
# Testing Dataset
raw_data_2018<-raw_data_2018 %>%
  mutate(agecat = ifelse(is.na(agecat),
                         ifelse(18<=age & age<28,1,
                                ifelse(28<=age & age<38,2,
                                       ifelse(38<=age & age<48,3,
                                              ifelse(48<=age & age<58,4,
                                                     ifelse(58<=age & age<68,5,6))))),agecat))

# Now 89% of the observation have all values 
aggr_plot <- aggr(raw_data_2017, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(raw_data_2017), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# Variables sorted by number of missings: 
# Variable      Count
# traffic_index 0.05800437
# credit_score  0.04638032

# Keeping only relevant columns ftr the imputing process
raw_data_2017<-raw_data_2017[,c("gender","age","agecat","credit_score","area","traffic_index","veh_age","veh_body","veh_value")]
raw_data_2018<-raw_data_2018[,c("gender","age","agecat","credit_score","area","traffic_index","veh_age","veh_body","veh_value")]

# Correcting Data Types
raw_data_2017$gender<-as.factor(raw_data_2017$gender)
raw_data_2017$agecat<-as.factor(raw_data_2017$agecat)
raw_data_2017$area<-as.factor(raw_data_2017$area)
raw_data_2017$veh_body<-as.factor(raw_data_2017$veh_body)
raw_data_2018$gender<-as.factor(raw_data_2018$gender)
raw_data_2018$agecat<-as.factor(raw_data_2018$agecat)
raw_data_2018$area<-as.factor(raw_data_2018$area)
raw_data_2018$veh_body<-as.factor(raw_data_2018$veh_body)


# Imputing the missing data
combined_data<-rbind(raw_data_2017,raw_data_2018)
init = mice(combined_data, maxit=0) 
meth = init$method
meth[c("gender")]=""
meth[c("age")]=""
meth[c("agecat")]=""
meth[c("area")]=""
meth[c("veh_age")]=""
meth[c("veh_body")]=""
meth[c("veh_value")]=""
meth[c("credit_score")]="pmm"
meth[c("traffic_index")]="pmm"
imputed = mice(combined_data, method=meth, m=1,maxit = 5,seed = 500)
imputed <- complete(imputed)

# write.csv(imputed,"imputed.csv")
# imputed<-read.csv("imputed.csv", row.names = NULL)

index<-1:60392
imp_2017<-imputed[index,]
imp_2018<-imputed[-index,]

imp_2017<-cbind(imp_2017,numclaims,claimcst0)
imp_2017$claim_YN<-as.factor(ifelse(claimcst0>0,1,0)) # Response needs to be a category
#################################################### Data Exploration #################################################### 
# Gender
imp_2017 %>%
  select(gender,claim_YN) %>%
  group_by(gender,claim_YN) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x=gender,y=cnt,fill = factor(claim_YN))) + 
  geom_bar(stat="identity",position = "fill") +
  ggtitle("Claims by Gender") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("") + ylab("Proportion")

# Age
imp_2017 %>%
  select(age, claim_YN) %>%
  group_by(age,claim_YN) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x=age,y=cnt,fill=factor(claim_YN))) + 
  geom_bar(stat = "identity",position = "fill") +
  ggtitle("Claim counts by Age") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("") + ylab("Proportion")

# Age Category
imp_2017 %>%
  select(agecat,claim_YN) %>%
  group_by(agecat,claim_YN) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x=agecat,y=cnt,fill = factor(claim_YN))) + 
  geom_bar(stat="identity",position = "fill") +
  ggtitle("Claims by Age Category") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("") + ylab("Proportion")

# Credit Score
ggplot(imp_2017, aes(x=credit_score,fill = factor(claim_YN))) + 
  geom_histogram(position = "stack", binwidth=20) +
  ggtitle("Claim by Credit Score") + theme(plot.title = element_text(hjust=0.5)) + 
  xlab("")

# Area
imp_2017 %>%
  select(area, claim_YN) %>%
  group_by(area,claim_YN) %>%
  summarise(cnt = n()) %>%
ggplot(aes(x=area,y=cnt,fill = factor(claim_YN))) + 
  geom_bar(stat = "identity",position = "fill") +
  ggtitle("Claim by Area") + theme(plot.title = element_text(hjust=0.5)) + 
  xlab("") + ylab("Proportion")

# Traffic
ggplot(imp_2017, aes(x=traffic_index,fill = factor(claim_YN))) + 
  geom_histogram(position = "stack", binwidth=20) +
  ggtitle("Claim by Traffic Index") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("")

# Vehicle Age
imp_2017 %>%
  select(veh_age, claim_YN) %>%
  group_by(veh_age,claim_YN) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x=veh_age,y=cnt,fill = factor(claim_YN))) + 
  geom_bar(stat = "identity",position = "fill") +
  ggtitle("Claim by Vehicle Age") + theme(plot.title = element_text(hjust=0.5)) + 
  xlab("") + ylab("Proportion")

# Vehicle Body
imp_2017 %>%
  select(veh_body, claim_YN) %>%
  group_by(veh_body, claim_YN) %>%
  summarise(cnt = n()) %>%
  arrange(cnt) %>%
  ggplot(aes(x=reorder(veh_body,-cnt), y = cnt, fill = factor(claim_YN))) + 
  geom_bar(stat = "identity",position = "fill") +
  ggtitle("Claim by Vehicle Body") + theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle = 90)) +
  xlab("") + ylab("Proportion")

# Vehicle Value
vehicle_values_approx<-imp_2017[imp_2017$veh_value != 0,] %>% # Changing vehicles with 0 value to max of their vehicle_age + vehicle_body category
  select(veh_body,veh_value) %>%
  group_by(veh_body) %>% 
  summarise(max_veh_val = max(veh_value))

imp_2017<-imp_2017 %>%
  mutate(veh_value = ifelse(veh_value==0,
                            ifelse(veh_body=="BUS",7.1830,
                                   ifelse(veh_body=="MCARA",13.6400,
                                          ifelse(veh_body=="MIBUS",5.0160,
                                                 ifelse(veh_body=="SEDAN",24.9590,
                                                        ifelse(veh_body=="STNWG",12.5400,6.2700))))),veh_value))

imp_2018<-imp_2018 %>%
  mutate(veh_value = ifelse(veh_value==0,
                            ifelse(veh_body=="BUS",7.1830,
                                   ifelse(veh_body=="MCARA",13.6400,
                                          ifelse(veh_body=="MIBUS",5.0160,
                                                 ifelse(veh_body=="SEDAN",24.9590,
                                                        ifelse(veh_body=="STNWG",12.5400,6.2700))))),veh_value))


ggplot(imp_2017[imp_2017$veh_value<=7.5,], aes(x=veh_value,fill = factor(claim_YN))) + 
  geom_histogram(position = "stack", binwidth=.01) +
  ggtitle("Claim by Vehicle value") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("")

# Claims amount
imp_2017 %>%
  filter(claimcst0 != 0) %>%
  select(claimcst0) %>%
  mutate(quantile = ntile(claimcst0, 100)) %>%
  filter(quantile<=90) %>%   # Removing high 10%iles because they are skewing the graph
  ggplot(aes(claimcst0)) + 
  geom_histogram(binwidth = 100,fill = "#F8766D", col = "black") +
  scale_x_continuous(breaks = seq(80,9000,800)) +
  ggtitle("Count by Claim amounts") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("")

# Claims: Yes/No
imp_2017 %>%
  select(claim_YN) %>%
  group_by(claim_YN) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x=factor(claim_YN),y=cnt,fill = factor(claim_YN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Count by Claims") + theme(plot.title = element_text(hjust=0.5)) + 
  xlab("") 

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::::::::::: CLAIMS: YES OR NO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#


# Correlation
cor_data<-imp_2017[,c(2,4,6,7,9,10,11)]
M<-cor(cor_data)
corrplot(M,method="number")
# Vehicle value and vehicle age have a very high correlation
# Credit score have mild correlations with age, claim counts and amounts


ggplot(imp_2017,aes(x=age,y=credit_score)) + geom_jitter(aes(colour = factor(claim_YN)))
# Young people have range of credit score
# Old people tend to have higher credit score
# Not much relation of age,crdit score combination on the claims
set.seed(500)
# Training & Validation
index<-sample(nrow(imp_2017),0.8*nrow(imp_2017), replace = FALSE)
training<-imp_2017[index,-c(10,11)] # Removing the "numclaims" & "claimcst0"
testing<-imp_2017[-index,-c(10,11)] # Removing the "numclaims" & "claimcst0"

# 16% claims in both the sets: Comparatively balanced dataset
table(training$claim_YN)[2]/(table(training$claim_YN)[2]+table(training$claim_YN)[1])
table(testing$claim_YN)[2]/(table(testing$claim_YN)[2]+table(testing$claim_YN)[1])

# ********************** <<<< LOGISTIC REGRESSION >>> ********************** # 
log.fit1<-glm(claim_YN ~ .,family=binomial(link='logit'),data=training)
vif(log.fit1) 
# age and agecat correlation is inflating the variance
# either of one needs to be removed: agecat will be removed

log.fit2<-glm(claim_YN ~ .-agecat,family=binomial(link='logit'),data=training)
log.fit3<-glm(claim_YN ~ .-agecat -veh_age -veh_body ,family=binomial(link='logit'),data=training)
vif(log.fit1);vif(log.fit2);vif(log.fit3)
summary(log.fit1);summary(log.fit2);summary(log.fit3)
AIC(log.fit1);AIC(log.fit2);AIC(log.fit3)
# We will sleetc model 2 becuase: (1) it doesnt contain the correlated variables and (2) Its AIC value is lower than model 3
log.pred<-predict(log.fit2,testing,type = "response")



# ********************** <<<< RANDOM FOREST >>> **************************** #  
set.seed(500)
rf.fit<-randomForest(claim_YN ~ .-agecat,ntree = 100, data = training)
plot(rf.fit)
varImpPlot(rf.fit,type=2, main = "Credit Score and Traffic Index are the most important predictors")
# Surprisingly veh_value is coming important in random forest and non-significant in logistic regression
# Means veh_value might have some non-linear relationship with response claim_YN
rf.pred<-predict(rf.fit,testing,type = "prob")[,2]

# ********************** <<<< XGBOOST >>> ********************************** #  
predictors<-c("gender","age","credit_score","area","traffic_index","veh_age","veh_body","veh_value")
output_vector<-as.numeric(training[,"claim_YN"])-1
sparse_matrix_train <- sparse.model.matrix(claim_YN ~ ., data = training[,-3])[,-1]
sparse_matrix_test <- sparse.model.matrix(claim_YN ~ ., data = testing[,-3])[,-1]
set.seed(500)
xg.fit <- xgboost(data = sparse_matrix_train, label = output_vector, max_depth = 6,
               eta = 0.3, nthread = 2, nrounds = 200,objective = "binary:logistic")
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xg.fit)
xgb.plot.importance(importance_matrix = importance)
# Some more non-linear relationship of claim_YN with gender(Male) and age unearthed
xgb.pred<-predict(xg.fit, sparse_matrix_test)


# ********************** <<<< MODEL COMPARISON: AUC CURVES >>> ************* # 
# List of predictions
preds_list <- list(log.pred,rf.pred,xgb.pred)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(testing$claim_YN), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Validation Set ROC Curves")
legend(x = "bottomright", legend = c("Logistic", "Random Forest", "XGBoost"),fill = 1:m)

# AUC values
actual<-testing$claim_YN
log_auc<-auc(actual = actual, predicted = log.pred)
rf_auc<-auc(actual = actual, predicted = rf.pred)
xgb_auc<-auc(actual = actual, predicted = xgb.pred)
data.frame("Model" = c("Logistic Regression","Random Forest","XGBoost"),"AUC" = c(log_auc,rf_auc,xgb_auc))


# ********** <<<< FINAL PREDICTIONS AND POTENTIAL CUSTOMERS >>> ************* # 

# Finding best cutoff probability using gridsearch
pr<-seq(0,1,0.02)
accry<-c()
for(i in 1:length(pr)){
  log.pred<-ifelse(predict(log.fit2,testing,type = "response") >= pr[i],1,0)
  accry[i]<-sum(log.pred == testing$claim_YN)/nrow(testing)
}
log.tab<-data.frame(cbind("prob" = pr, "accuracy" = accry))
ggplot(log.tab,aes(x=prob,y=accuracy)) + 
  geom_point(show.legend=F) + 
  xlab("Threshold cutoff probability") + 
  ylab("Accuracy") + 
  ggtitle("Threshold Vs Accuracy (Logistic)") + theme(plot.title = element_text(hjust=0.5)) + 
  geom_point(aes(x=log.tab[log.tab$accuracy==max(log.tab$accuracy),][[1]],y=log.tab[log.tab$accuracy==max(log.tab$accuracy),][[2]],colour="red",cex=4))
# Cutoff probabiloity = 0.48
# Accuracy = 0.8522229

# Potential Customers
final_predictions<-data.frame(cbind("Quote Numbers" = quote_number,imp_2018,"Risk" = ifelse(predict(log.fit1,imp_2018,type = "response")>=0.48,1,0)))
table(final_predictions$Risk)
# 6891: potential non-risky customers
# 573: potential risky customers
potential_non_risky<-data.frame("Quote Numbers" = final_predictions[final_predictions$Risk==0,1])
potential_risky<-data.frame("Quote Numbers" = final_predictions[final_predictions$Risk==1,1])
write.csv(potential_non_risky,"potential_non_risky_customers.csv", row.names = FALSE)






#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#::::::::::::: COST PER CLAIM::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

claim_cust<-imp_2017 %>%
  mutate(cost_per_claim = ifelse(is.nan(claimcst0/numclaims),0,round((claimcst0/numclaims),2))) %>%
  select(-c(10:12)) %>%
  filter(cost_per_claim>0)
# 10030 customers 
# Correlation
cor_data<-claim_cust[,c(2,4,6,7,9,10)]
M<-cor(cor_data)
corrplot(M,method="number")
ggpairs(cor_data)
# Credit score, vehicle age and vehicle value have some relation with cost_per_claim
# Vehicle value and vehicle age are correlated

ggplot(claim_cust,aes(cost_per_claim)) + 
  geom_histogram(binwidth = 500,fill = "steelblue", col = "black") +
  scale_x_continuous(limits = c(min(claim_cust$cost_per_claim), 45000)) +
  ggtitle("Cost per claim is highly skewed") + theme(plot.title = element_text(hjust=0.5))
# Cost per claim is highyl skewed, we need log transformation (some variance stabilization)

claim_cust$cost_per_claim<-log(claim_cust$cost_per_claim)
ggplot(claim_cust,aes(cost_per_claim)) + 
  geom_histogram(fill = "steelblue", col = "black") +
  ggtitle("After log transformation") + theme(plot.title = element_text(hjust=0.5))

set.seed(500)
index<-sample(nrow(claim_cust),0.8*nrow(claim_cust),replace = FALSE)
training<-claim_cust[index,]
testing<-claim_cust[-index,]

# ***************** <<<< MULTIPLE LINEAR REGRESSION >>> ***************************** # 
lm.cpc.fit1<-lm(cost_per_claim ~ .,data = training)
lm.cpc.fit2<-lm(cost_per_claim ~ .- agecat,data = training) 
# Coefficient sign of age changed from -ve to +ve shwoing its collinearity with agecat
lm.cpc.fit3<-lm(cost_per_claim ~ .-veh_age -agecat,data = training)
# After removing, the positive coefficient of vehicle value increased (showing effect of collinearity with veh_age)
anova(lm.cpc.fit1,lm.cpc.fit2,lm.cpc.fit3)
# all three models suggests linear relationship of predictors with response

lm.mse1<-sum((predict(lm.cpc.fit1,testing)-testing$cost_per_claim)^2)/nrow(testing)
lm.mse2<-sum((predict(lm.cpc.fit2,testing)-testing$cost_per_claim)^2)/nrow(testing)
lm.mse3<-sum((predict(lm.cpc.fit3,testing)-testing$cost_per_claim)^2)/nrow(testing)
lm.mse1;lm.mse2;lm.mse3
# Full model seems to be superior in temrs of mean squared error o nthe validation set 
# So we choose model 1 (full model) 
# 0.4428091
par(mfrow=c(2,2))
plot(lm.cpc.fit1)
# Normality assumption seems to be valid (after log transformation)
par(mfrow=c(1,1))

# ***************** <<<< RANDOM FOREST >>> ***************************** # 
set.seed(500)
rf.cpc.fit1<-randomForest(cost_per_claim ~ ., ntree = 1000,data = training)
plot(rf.cpc.fit1)
varImpPlot(rf.cpc.fit1,type=2, main = "Credit Score and Vehicle age are the most important predictors")
rf.mse1<-sum((predict(rf.cpc.fit1,testing)-testing$cost_per_claim)^2)/nrow(testing)
# 0.4447331

set.seed(500)
rf.cpc.fit2<-randomForest(cost_per_claim ~ .-agecat, ntree = 1000,data = training)
plot(rf.cpc.fit2)
varImpPlot(rf.cpc.fit2,type=2, main = "Credit Score and Vehicle age are the most important predictors")
rf.mse2<-sum((predict(rf.cpc.fit2,testing)-testing$cost_per_claim)^2)/nrow(testing)
# 0.4536418
# removing the correlated term doesnt improve the error

# ***************** <<<< XGBOOST >>> ***************************** # 
output_vector<-as.numeric(training[,"cost_per_claim"])
sparse_matrix_train <- sparse.model.matrix(cost_per_claim ~ ., data = training)[,-1]
sparse_matrix_test <- sparse.model.matrix(cost_per_claim ~ ., data = testing)[,-1]
set.seed(500)
xg.fit1 <- xgboost(data = sparse_matrix_train, label = output_vector, max_depth = 6,
                  eta = 0.3, nthread = 2, nrounds = 200,objective = "reg:linear")
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xg.fit1)
xgb.plot.importance(importance_matrix = importance)
xgb.mse1<-sum((predict(xg.fit1, sparse_matrix_test)-testing$cost_per_claim)^2)/nrow(testing)
# 0.4736492

sparse_matrix_train <- sparse.model.matrix(cost_per_claim ~ ., data = training[,-3])[,-1]
sparse_matrix_test <- sparse.model.matrix(cost_per_claim ~ ., data = testing[,-3])[,-1]
set.seed(500)
xg.fit2 <- xgboost(data = sparse_matrix_train, label = output_vector, max_depth = 6,
                   eta = 0.3, nthread = 2, nrounds = 200,objective = "reg:linear")
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xg.fit2)
xgb.plot.importance(importance_matrix = importance)
xgb.mse2<-sum((predict(xg.fit2, sparse_matrix_test)-testing$cost_per_claim)^2)/nrow(testing)
# 0.4699062


sparse_matrix_train <- sparse.model.matrix(cost_per_claim ~ ., data = training[,-c(3,7)])[,-1]
sparse_matrix_test <- sparse.model.matrix(cost_per_claim ~ ., data = testing[,-c(3,7)])[,-1]
set.seed(500)
xg.fit3 <- xgboost(data = sparse_matrix_train, label = output_vector, max_depth = 6,
                   eta = 0.3, nthread = 2, nrounds = 200,objective = "reg:linear")
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xg.fit3)
xgb.plot.importance(importance_matrix = importance)
xgb.mse3<-sum((predict(xg.fit3, sparse_matrix_test)-testing$cost_per_claim)^2)/nrow(testing)
# 0.6076743
# Removing veh_age not a good idea in xgboost



# ********** <<<< FINAL PREDICTIONS: CUSTOMERS WITH LOW COST PER CLAIMS >>> ************* # 
potential_risky$Quote.Numbers
potential_risky<-data.frame(cbind(potential_risky,
                       "Cost_Per_Claim" = predict(lm.cpc.fit1,final_predictions[final_predictions$Quote.Numbers %in% potential_risky$Quote.Numbers,-c(1,11)]))
                            )
potential_risky$real_cost_per_claim<-exp(potential_risky$Cost_Per_Claim)

low_risky<-potential_risky %>%
  mutate(quantile = ntile(real_cost_per_claim, 100)) %>%
  filter(quantile<=15) %>% # Selecting las t15%iles to be low-risky customers (selection of cutoff depends on business)
  select(Quote.Numbers)

write.csv(low_risky,"potential_low_risky_customers.csv",row.names = FALSE)




#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#::::::::::::: RISK PROFILING :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
features<-c("Quote.Numbers","gender","age","credit_score","area","traffic_index",
            "veh_age","veh_body","veh_value","Risk","Cost_Per_Claim","real_cost_per_claim") 

cust_all_risk<-final_predictions %>%
  mutate("Risk_Prob" = predict(log.fit1,imp_2018,type = "response")) %>%
  # select(c("Quote.Numbers","gender","age","credit_score","veh_body","veh_value","traffic_index","Risk","Risk_Prob")) %>%
  select(c("Quote.Numbers","age","credit_score","veh_value","traffic_index","Risk_Prob"))
  filter(Risk == 1)
  
# "gender"        "age"           "credit_score"  "veh_value"     "traffic_index" "Risk"          "Risk_Prob"     
# Calculate Gower Distance
gower_dist <- daisy(cust_all_risk[,-1],metric = "gower", type = list(logratio = c(1,2,3,4,5))) # , weights = c(2,3,7,4,5,7,0,8)
# Log transformation for positively skewed variables: FAMILY_TOT_SALES, FAMILY_TOT_VISITS


# Calculate optimal number of clusters
sil_width <- c(NA)
for(i in 2:13){
  set.seed(i)
  pam_fit<-pam(gower_dist, diss = TRUE,k = i)  # PAM: Partitioning Around Medoids 
  sil_width[i]<-pam_fit$silinfo$avg.width
}
tab<-data.frame(x=1:13,sil_width=sil_width)


ggplot(data=tab,aes(x = x,y = sil_width)) + 
  geom_point(cex=3,col="red")+geom_line() + 
  ggtitle("Silhoutte Width Vs Number of clusters") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  xlab("Number of clusters")
# Number of clusters suggested by silhoutte analysis: 5

# Creating clusters
pam_fit<-pam(gower_dist, diss=TRUE, k = 5)
cust_all_risk<-cbind(cust_all_risk, Group = pam_fit$clustering)

# Visualizing the clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = cust_all_risk$Quote.Numbers)

ggplot(aes(x = X, y = Y), data = tsne_data) + 
  geom_point(aes(color = cluster)) + 
  ggtitle("Customer Segments") + 
  theme(plot.title = element_text(hjust = 0.5))


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


cust_all_risk %>%
  group_by(Group) %>%
  summarise(count_cust = n(),avg_age = mean(age)
                            ,min_prob =  min(Risk_Prob), med_prob = median(Risk_Prob), max_prob = max(Risk_Prob)
                            ,min_cred_scr =  min(credit_score), med_cred_scr = median(credit_score), max_cred_scr = max(credit_score)
                            ,min_tf_in =  min(traffic_index), med_tf_in = median(traffic_index), max_tf_in = max(traffic_index)
                            ,mod_veh = Mode(veh_body)
                            )