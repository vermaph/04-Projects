##############################
## Modelling
##############################


library("caret")
library("boot")     # For calcualting errors from cross-validation 
library("rpart")    # For random forest
library("rattle")   # For visualizing the random forest tree
library("ggplot2")  # For visualizations
library("dplyr")    # For streamlining the code for manipulating the datasset 
library("cluster")   
library("Rtsne")    # For visualizing the clustering in 2-D
library("tibble")   
library("xgboost")  # For Xgboost
library("glmnet")   # For Lasso
library("neuralnet")# For neuralnet
library("reshape2") # For boxplots
# Here only the FAMILY_TOT_SALES is of interest
# We will fit linear, random forest, xgboost and neural network
# We will use cross-validation to calculate error and tune model

library("readxl")
Customer_Data <- read_excel("C:/0000/05 Github/Codes/Pet Projects/Supermarket dashboard/Customer Data.xlsx")
View(Customer_Data)
names<-c(2:8)
Customer_Data[,names]<-lapply(Customer_Data[,names],factor)
Customer_Data$INCOME_DESC<-factor(Customer_Data$INCOME_DESC, levels = c("Under 15K","15-24K","25-34K",
"35-49K","50-74K","75-99K","100-124K","125-149K","150-174K","175-199K","200-249K","250K+"))
Customer_Data_Model<-Customer_Data[,-c(1,10,11)]   # Removing FAMILY_TOT_VISITS & FAMILY_VALUE

############################## Linear regression ############################################################
lm.err<-c(NA)
for(i in 1:10){
  set.seed(i)
  fit<-glm(FAMILY_TOT_SALES ~ .,data = Customer_Data_Model) # Fits a linear regression
  lm.err[i]<-cv.glm(Customer_Data_Model,fit,K=10)$delta[2]  # Taking the Bias adjusted error
}  
lm.err<-sqrt(mean(lm.err)) # Mean Squared Error: After running 10-K fold cross-validation 10 times, very high 
# Error: 1797.141
par(mfrow=c(2,2))
plot(fit) 
# Linear assumption of normality is violated
# Also we can't do non-linear models because we don't have any numeric predictors in the dataset, 
# only the response variable is numeric
# We can do linear regression with predictors as polynomials but because advanced modelling techniques like 
# random forest, xgboost and neural networks generally give better accuracy by sacrificing interpretability
# we will not do polynomials here.
# But F-statistic for linear model atleast tells us that there is some relationship between predictors and response
# as p-value is very small for the same

set.seed(05052018)
index<-sample(1:nrow(Customer_Data_Model),nrow(Customer_Data_Model)*0.8,replace = TRUE)
training<-Customer_Data_Model[index,]
testing<-Customer_Data_Model[-index,]

null<-lm(FAMILY_TOT_SALES~1,data = training)
full<-lm(FAMILY_TOT_SALES~.,data = training)
step.fit<-step(null,scope = list(upper = full, lower = null),direction = "both",trace = TRUE, k = 2)
lm.AIC<-lm(FAMILY_TOT_SALES ~ INCOME_DESC + KID_CATEGORY_DESC,data=training)
lm.BIC<-lm(FAMILY_TOT_SALES ~ INCOME_DESC,data=training)

lm.AIC.err<-sqrt(sum((predict(lm.AIC,testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
lm.BIC.err<-sqrt(sum((predict(lm.BIC,testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))

# AIC best model: 
# FAMILY_TOT_SALES ~ INCOME_DESC + KID_CATEGORY_DESC
# 1714.12
  
# BIC best model: 
# FAMILY_TOT_SALES ~ INCOME_DESC
# 1730.093


################################ Lasso Regression ###########################################################
y.train<-as.matrix(training[,8])   # Response in training has to be in matrix form for glmnet 
x.train<-as.matrix(model.matrix(FAMILY_TOT_SALES ~.,training)[,-1]) 
# Predictors in training has to be in matrix form for glmnet 
# model.matrix creates dummy variable for each level of each categorical variable 
cv.lasso<-cv.glmnet(x = x.train,y = y.train, alpha = 1)
lasso<-glmnet(x = x.train,y = y.train, alpha = 1)
plot(cv.lasso)  # Plots: MSE vs Lambda
plot(lasso)   # Plots: Coefficients vs Lambda

best_lam<-cv.lasso$lambda.min  # Best lambda that gives minimum mse


x.test<-as.matrix(model.matrix(FAMILY_TOT_SALES ~.,testing)[,-1]) 
y.test<-as.matrix(testing[,8]) 
newdata<-as.matrix(data.frame(cbind(x.test,y.test)))
coef(lasso,s=best_lam)
# Marital status was removed during lasso regression
lasso.fit<-lm(FAMILY_TOT_SALES ~ . - MARITAL_STATUS_CODE, data = training)

lasso.err<-sqrt(sum((predict(lasso.fit, testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
# Error: 1740.837
#Out of all the linear models, the one obtained from AIC criteria is so far the bets one




############################### Random forest ############################################################### 

rf.fit1<-rpart(FAMILY_TOT_SALES ~ .,data = training)
plotcp(rf.fit1)
# two splits are suggested based on complexity parameter
rf.fit2<-prune(rf.fit1,cp = 0.041)
fancyRpartPlot(rf.fit2)
rf.err<-sqrt(sum((predict(rf.fit2,testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
# Error: 1772.224
# Error has improved marginally and still it is unacceptable


rf.fit3<-prune(rf.fit1,cp = 0.013)
fancyRpartPlot(rf.fit3)
rf.err<-sqrt(sum((predict(rf.fit3,testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
varImp(rf.fit3)


barplot(table(training$INCOME_DESC), main = "Counts of customers by Income group")
income<-Customer_Data_Model %>% 
  group_by(INCOME_DESC) %>%
  summarize(Median_Sales_By_Income = median(FAMILY_TOT_SALES))
attach(income)
plot(INCOME_DESC,Median_Sales_By_Income, main = "Annual household sales by Income group")
abline(h=2330, col = "blue")
abline(h=3898,col = "red")
# High prediction value in the other group is driven by one group of people who are less n number but have 
# shopped in large

rf.err<-sqrt(sum((predict(rf.fit3,testing) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
# 1828.753  


############################### XGBoost ###################################################################
sparse_matrix <- sparse.model.matrix(FAMILY_TOT_SALES ~ .-1, data = training) # One Hot Encoding
y = training$FAMILY_TOT_SALES

xgb.fit<-xgboost(data = sparse_matrix,     # Predictors in sparse matrix
                 label = y,                # Response as it is
                 booster = "gbtree",       
                 eta = 0.1,                
                 max_depth = 15,           # Dept hof the tree
                 nround=25,                # Number of trees/iterations
                 nfold = 10,               # Number of folds in K-fold
                   
                 objective = "reg:linear", # Default option for xgboost
                 eval_metric = "rmse",
                 nthread = 3,              # Number of cores to be run parallelly for xgboost
                 early.stop.round = 10     # Stop if model doesnt improve after 10 iterations
                )


train.xgb<- as.matrix(training, rownames.force=NA)
test.xgb<- as.matrix(testing, rownames.force=NA)
train.xgb <- as(train.xgb, "sparseMatrix")
test.xgb <- as(test.xgb, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train.xgb[,1:7], label = train.xgb[,"FAMILY_TOT_SALES"])


param<-list(
            objective = "reg:linear",   # Objective to minimize
            eval_metric = "rmse",       # Evaluation metric fro validating the model
            booster = "gbtree",         # Default option for xgboost
            max_depth = 8,              # Depth of the tree
            eta = 0.123                 # Learning rate: weight
           )
xgb.fit<-xgb.train(params = param,
                        data = train_Data,
                        nrounds = 200,
                        watchlist = list(train = train_Data),
                        verbose = TRUE,
                        print_every_n = 10,
                        nthread = 6)

test_data <- xgb.DMatrix(data = test.xgb[,1:7])
xgb.err<-sqrt(sum((predict(xgb.fit,test_data) - testing$FAMILY_TOT_SALES)^2)/nrow(testing))
# 1752.074
############################## Neural Network #############################################################


m<-model.matrix( ~ AGE_DESC + MARITAL_STATUS_CODE + INCOME_DESC + 
                  HOMEOWNER_DESC + HH_COMP_DESC + HOUSEHOLD_SIZE_DESC + KID_CATEGORY_DESC + FAMILY_TOT_SALES, data = training)[,-1]     
n<-colnames(m)
# Neuralnet works only with quantitative variabels, one hot encoding is a must
# Formula was created using: paste(n,collapse = '` + `')

# All features
#f<-as.formula("FAMILY_TOT_SALES ~ `AGE_DESC25-34` + `AGE_DESC35-44`+ `AGE_DESC45-54` + `AGE_DESC55-64` + `AGE_DESC65+` + `MARITAL_STATUS_CODEB` + `MARITAL_STATUS_CODEU` + `INCOME_DESC15-24K` + `INCOME_DESC25-34K` + `INCOME_DESC35-49K`+ `INCOME_DESC50-74K` + `INCOME_DESC75-99K` + `INCOME_DESC100-124K` + `INCOME_DESC125-149K` + `INCOME_DESC150-174K` + `INCOME_DESC175-199K` + `INCOME_DESC200-249K` + `INCOME_DESC250K+` + `HOMEOWNER_DESCProbable Owner` + `HOMEOWNER_DESCProbable Renter` + `HOMEOWNER_DESCRenter` + `HOMEOWNER_DESCUnknown` + `HH_COMP_DESC2 Adults Kids` + `HH_COMP_DESC2 Adults No Kids`+ `HH_COMP_DESCSingle Female` + `HH_COMP_DESCSingle Male` + `HH_COMP_DESCUnknown` + `HOUSEHOLD_SIZE_DESC2` + `HOUSEHOLD_SIZE_DESC3` + `HOUSEHOLD_SIZE_DESC4` + `HOUSEHOLD_SIZE_DESC5+`+ `KID_CATEGORY_DESC2` + `KID_CATEGORY_DESC3+` + `KID_CATEGORY_DESCNone/Unknown`")

# Selected significant Features from linear model
f<-as.formula("FAMILY_TOT_SALES ~ `INCOME_DESC15-24K` + `INCOME_DESC25-34K` + `INCOME_DESC35-49K`+ `INCOME_DESC50-74K` + `INCOME_DESC75-99K` + `INCOME_DESC100-124K` + `INCOME_DESC125-149K` + `INCOME_DESC150-174K` + `INCOME_DESC175-199K` + `INCOME_DESC200-249K` + `INCOME_DESC250K+` + `HOUSEHOLD_SIZE_DESC2` + `HOUSEHOLD_SIZE_DESC3` + `HOUSEHOLD_SIZE_DESC4` + `HOUSEHOLD_SIZE_DESC5+`+ `KID_CATEGORY_DESC2` + `KID_CATEGORY_DESC3+` + `KID_CATEGORY_DESCNone/Unknown`")
nn<-neuralnet(f,data=m,hidden=c(4),linear.output=T, stepmax=1e6)
# Number of layers: Generally one layer is sufficient
# Number of nodes: Rule of thumb is the average of input & output nodes
# Neural Netwrok is outputting the average values of all FAMILT_TOT_SALES = 2578.973359
# Meaning Neural Netowkr is not useful here or the predictors are not meaningful, or the model is stuck at a local minima


# Checking with another package
nn2<-nnet(mm,m[,35],size=10,linout=T)
predict(nn2,m[,-8])
# 2578.973349 for all observations

############################## Clustering of the Customers ################################################ 
## Calculate Gower Distance
gower_dist <- daisy(Customer_Data[,-1],metric = "gower", type = list(logratio = c(8,9,10))) 
# Log transformation for positively skewed variables: FAMILY_TOT_SALES, FAMILY_TOT_VISITS


## Calculate optimal number of clusters
sil_width <- c(NA)
for(i in 2:20){
pam_fit<-pam(gower_dist, diss = TRUE,k = i)  # PAM: Partitioning Around Medoids 
sil_width[i]<-pam_fit$silinfo$avg.width
}
tab<-data.frame(x=1:20,sil_width=sil_width)
ggplot(data=tab,aes(x = x,y = sil_width))+geom_point(cex=3,col="red")+geom_line()+ggtitle("Silhoutte Width Vs Number of clusters")+theme(plot.title = element_text(hjust=0.5))+xlab("Number of clusters")


## Creating clusters
pam_fit<-pam(gower_dist, diss=TRUE, k = 8)
Customer_Data<-cbind(Customer_Data, Group = pam_fit$clustering)

## Visualizing the clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = Customer_Data$H_KEY)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster)) + ggtitle("Customer Segments") + theme(plot.title = element_text(hjust = 0.5))

Customer_Data2<-melt(Customer_Data,Group=Group,measure.vars = FAMILY_TOT_SALES)
ggplot(data=Customer_Data)+geom_boxplot(aes(x=Group,y=FAMILY_TOT_SALES,color = Group))+facet_wrap(~Group)


result<-Customer_Data %>% 
  group_by(Group) %>% 
  summarize(Avg_sales = mean(FAMILY_TOT_SALES), Avg_visits = mean(FAMILY_TOT_VISITS)) %>%
  arrange(desc(Avg_sales),desc(Avg_visits))



