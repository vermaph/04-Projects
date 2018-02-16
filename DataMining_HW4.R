########################## 

## Data Mining I
## Home Work 4
## 02/09/18
## Piyush Verma, Group-15

##########################


############# Loading Libraries

library("ggplot2")
library("MASS")
library("corrplot")
library("caret")
library("leaps")
library("glmnet")
library("dplyr")
library("randomcoloR")
library("reshape2")
library("rpart")
library("rattle")
library("corrplot")
library("Deducer")
library("ROCR")
library("boot")

#############



################ Problem 1: Boston Data


## Step 1 
          ##(a): Splitting the data
          ##(b): Exploratory Data Analysis
          ##(c): Conduct Linear Regression
          set.seed(1234)
          index<-sample(nrow(Boston),0.8*nrow(Boston),replace = FALSE)
          training<-Boston[index,]
          testing<-Boston[-index,]
          
          #Histograms: Distributions
          training2 <- melt(training[, sapply( training, class) %in% c( "numeric", "integer")])
          ggplot(data = training2, aes(value, fill = variable)) + geom_histogram(bins = 10) + facet_wrap(~ variable, scales = "free_x") + ggtitle("Histograms: Checking distributions of all variables") + theme(plot.title=element_text(hjust=0.5))
          
          #Boxplots: Outliers
          ggplot(data = training2, aes(x=variable,y=value, fill = variable))+geom_boxplot() + ggtitle("Boxplot: Checking outliers for all variables") + facet_grid(~ variable, scales = "free_x") + theme(plot.title=element_text(hjust=0.5))
          
          
          lm_obj<-lm(medv~.,data=training)




## Step 2: Best Model Selection 
          ##(a): using AIC, BIC, and Lasso regression
          ##(b): Report model mean squared error
          ##(c): Conduct some residual diagnosis
          
          
          
          
          #AIC
          AIC_obj<-stepAIC(lm(medv~1,data=training),
                       scope = list(lower= ~1, upper = ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat),
                       direction = c("both"),
                       trace = TRUE)
          AIC_obj$anova 
          #Final Model, 11 predictors:
          ##  medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + rad + tax + crim
          AIC_mod<-lm(medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + rad + tax + crim, data=training)
          AIC(AIC_mod)
          #2440.282
          
          AIC_mse<-sum(AIC_mod$residuals^2)/(AIC_mod$df.residual)
          AIC_mse
          #23.76517
          AIC_out_mse<-mean( (testing$medv - predict(AIC_mod,testing) )^2 )
          AIC_out_mse
          #18.68302
          
          
          
          
          #Best Model / BIC
          best_model<-regsubsets(medv~.,data=training,nvmax=19)
          plot(best_model, scale="bic")
          
          best_model_summ<-summary(best_model)
          #Final Model, 10 predictors:
          #medv ~ zn + chas + nox + rm +dis + rad + tax + ptratio + black + lstat
          BIC_mod<-lm(medv ~ zn + chas + nox + rm +dis + rad + tax + ptratio + black + lstat, data = training)
          BIC(BIC_mod)
          #2489.472
          
          
          
          
          BIC_mse<-sum(BIC_mod$residuals^2)/(BIC_mod$df.residual)
          BIC_mse
          #23.89159
          BIC_out_mse<-mean( (testing$medv - predict(BIC_mod,testing) )^2 )
          BIC_out_mse
          #20.19618
          
          
          
          
          #Lasso Regression
          training_std<-scale(dplyr::select(training, -medv))
          X.train<-as.matrix(training_std)[,]
          Y.train<-training[,"medv"]
          
          testing_std<-scale(dplyr::select(testing, -medv))
          X.test<-as.matrix(testing_std)[,]
          Y.test<-testing[,"medv"]

              #Coefficient vs Lambda
              lasso.mod<- glmnet(x=X.train, y=Y.train, alpha = 1)  #alpha = 1 is lasso fit
              plot(lasso.mod, xvar = "lambda")
              
              #CV Error vs Lambda
              lasso.cv<-cv.glmnet(x=X.train, y=Y.train, alpha = 1, nfolds = 10)        
              plot(lasso.cv)
              
              #Predictions of medv based on best lambda
              best_lam<-lasso.cv$lambda.min
              lasso_pred<-predict(lasso.mod,s=best_lam,newx = X.test)
              

              
              coef(lasso.mod, s=best_lam)
              #Lasso is using all the variables
              
              
              lasso_mse<-sum(lm_obj$residuals^2)/(lm_obj$df.residual)
              lasso_mse
              #23.85523
              lasso_out_mse<-mean( (testing$medv - predict(lm_obj,testing) )^2 )
              lasso_out_mse
              #
              
              
              #Residual Diagnostics
              par(mfrow=c(1,3),oma=c(0,0,2,0))
              plot(AIC_mod,1,main="AIC");plot(BIC_mod,1,main = "BIC");plot(lm_obj2,1,main = "Lasso")
       
              par(mfrow=c(1,3),oma=c(0,0,2,0))
              plot(AIC_mod,2,main="AIC");plot(BIC_mod,2,main = "BIC");plot(lm_obj2,2,main = "Lasso")
              
              par(mfrow=c(1,3),oma=c(0,0,2,0))
              plot(AIC_mod,3,main="AIC");plot(BIC_mod,3,main = "BIC");plot(lm_obj2,3,main = "Lasso")
              
              par(mfrow=c(1,3),oma=c(0,0,2,0))
              plot(AIC_mod,4,main="AIC");plot(BIC_mod,4,main = "BIC");plot(lm_obj2,4,main = "Lasso")
## Step 3:
          ##(a): Test the out of sample performance for Linear Model in (i), using 20% test data set
          ##(b): Test the out of sample performance for Best Model in (ii), using 20% test data set

          lm_mse<-mean( (testing$medv - predict(lm_obj,testing))^2 )
          lm_mse
          #18.93611
              
          AIC_mse
          #18.68302

          
## Step 4:
          ##(a): Output out of sample error using 5 k fold cross validation on 100% of original data using model 1
          ##(b): Compare 4(a) vs 3(a) errors


          lm_obj2 = glm(medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + rad + tax + crim, data = Boston) #essentially AIC model on the complete Boston Dataset
          CV_mse<-cv.glm(Boston,lm_obj2, K = 5)$delta[2]
          CV_mse
          #22.99023
          
          AIC_out_mse
          #18.68302
          
          
## Step 5:
          ##(a): Fit CART. Report out-of-sample model MSE, using 20% test data set
         
          
          boston.rpart <- rpart(formula = medv ~ ., data = training)
          fancyRpartPlot(boston.rpart)

          tree_mse<-mean( (testing$medv - predict(boston.rpart,testing))^2 )
          tree_mse
          #22.6584

          
## Step 6:
          ##(a): Compare CART vs Linear Model in (i)

          tree_mse
          #22.6584
          
          AIC_mse
          #18.68302
          
          
          
## Step 7:
          ##(a): Generate new training and test dataset
          ##(b): Compare linear model (i), best model (ii)
          
          set.seed(4321)
          index<-sample(nrow(Boston),0.8*nrow(Boston),replace = FALSE)
          training<-Boston[index,]
          testing<-Boston[-index,]
          lm_obj<-lm(medv~.,data=training)
          AIC_obj<-stepAIC(lm(medv~1,data=training),
                           scope = list(lower= ~1, upper = ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat),
                           direction = c("both"),
                           trace = TRUE)
          AIC_mod<-lm(medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + rad + tax + crim, data=training)
          AIC_mse<-mean( (testing$medv - predict(AIC_mod,testing) )^2 )
          best_model<-regsubsets(medv~.,data=training,nvmax=19)
          best_model_summ<-summary(best_model)
          BIC_mod<-lm(medv ~ zn + chas + nox + rm +dis + rad + tax + ptratio + black + lstat, data = training)
          BIC_mse<-mean( (testing$medv - predict(BIC_mod,testing) )^2 )
          training_std<-scale(dplyr::select(training, -medv))
          X.train<-as.matrix(training_std)[,]
          Y.train<-training[,"medv"]
          testing_std<-scale(dplyr::select(testing, -medv))
          X.test<-as.matrix(testing_std)[,]
          Y.test<-testing[,"medv"]
          lasso.mod<- glmnet(x=X.train, y=Y.train, alpha = 1)  #alpha = 1 is lasso fit
          lasso.cv<-cv.glmnet(x=X.train, y=Y.train, alpha = 1, nfolds = 10)        
          best_lam<-lasso.cv$lambda.min
          lasso_pred<-predict(lasso.mod,s=best_lam,newx = X.test)
          Lasso_mse<-mean((lasso_pred - Y.test)^2)
          lm_mse<-mean( (testing$medv - predict(lm_obj,testing))^2 )
          lm_obj2 = glm(medv~., data = Boston) #essentially the model 1 
          CV_mse<-cv.glm(Boston,lm_obj2, K = 5)$delta[2]
          boston.rpart <- rpart(formula = medv ~ ., data = training)
          tree_mse<-mean( (testing$medv - predict(boston.rpart,testing))^2 )
          
          
          tree_mse
          #26.51272
          
          AIC_mse
          #19.64369
          
          BIC_mse
          #20.59811
          
          CV_mse
          #23.76596
          
          Lasso_mse
          #20.79363
          
          lm_mse
          #19.86565
          
          
#Conclusion - Model by out-of-sample performance: 
# AIC  > lm_mse > BIC > Lasso_mse > CV_mse  > tree_mse
          
          
          
          
################ Problem 2: German Credit Data (Group 15 - Odd group)

          
## Step 1:
          ## (a): Split the data 80-20
          ## (b): Exploraoty data analysis. Fit logistic with different link functions (logistic, probit, cloglog). Compare models.

          set.seed(1234)
          url<-"http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
          German<-read.table(url)
          
          colnames(German) = c("chk_acct", "duration", "credit_his", "purpose", 
                                      "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                                      "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                                      "job", "n_people", "telephone", "foreign", "response")
          German$response = German$response - 1
          index<-sample(nrow(German),0.8* nrow(German), replace = TRUE)
          training<-German[index,]
          testing<-German[-index,]
          
          table(German$response) #30% of people in dataset have critical response 
          
          ## Histograms and Barplots
          par(mfrow=c(2,2))
          for(i in 1:ncol(German)-1){
            
            if(is.numeric(German[,i])){
              hist(German[,i],main = paste0(names(German)[i]),las = 2,xlab="")
            }
            if(is.factor(German[,i]))
            {
              plot(German[,i],main = paste0(names(German)[i]),las = 2,xlab="")
            }
          }
          
    
          ## Boxplots
          par(mfrow=c(2,2))
          for(i in 1:(ncol(German)-1)){
            plot(German[,i],German$response,main = paste0(names(German)[i]),las = 2,xlab="")
          }
          
      
          #Coorelation matrix
          M<-cor(German[,which(sapply(German,is.numeric))])
          corrplot(M,method = "number", order = "FPC")
          # No numeric variables seem to be correlated except amount and duration, so in our final model they should not come together
          
          
          ##logistic
          
          log_mod<-glm(response~.,family=binomial(link="logit"),training)
          #AIC: 774.8
          #Residual deviance: 676.8023
          
          prob_mod<-glm(response~.,family=binomial(link="probit"),training)
          #AIC: 773.71
          #Residual deviance: 675.7069
          
          clog_mod<-glm(response~.,family=binomial(link="cloglog"),training)
          #AIC: 771.63
          #Residual deviance: 673.6261
          
          par(mfrow=c(1,3),oma=c(0,0,2,0))
          hist(predict(log_mod,type = "response"), main = "Logistic", xlab="")
          hist(predict(prob_mod,type = "response"), main = "Probit", xlab="")
          hist(predict(clog_mod,type = "response"), main = "Cloglog", xlab="")
          title(main = "Comparing distribution of predictios from 3 different link functions", outer = TRUE)
      
          
          
          
          
## Step 2:
          ## (a): Best model using AIC, BIC and lasso variable selection
          ## (b): Draw ROC
          ## (c): Report mean residual deviance, AUC, Misclassification rate 
          
          
          #AIC
          null<-glm(response~1,family = binomial(link="logit"),training)
          full<-glm(response~.,family = binomial(link="logit"),training)
          AIC_obj<-stepAIC(glm(response~1,data=training), scope = list(lower=null, upper=full),direction = c("both"),trace = TRUE)
          AIC_obj$anova
          AIC_mod<-glm(response ~ chk_acct + credit_his + duration + purpose + other_debtor + 
                         saving_acct + other_install + foreign + present_emp + amount + 
                         property + housing + installment_rate + sex + present_resid, family = binomial(link="logit"),training)
          summary(AIC_mod)
          AIC(AIC_mod)
          rocplot(AIC_mod)
          AIC_prob<-ifelse(predict(AIC_mod,testing,type=c("response"))>1/6,1,0)
          AIC_miss_rate<-mean( AIC_prob!=testing$response )
          AIC_miss_rate
          #15 predictors
          #Mean residual deviance: 705.54
          #AUC: 0.8421
          #Misclassification rate: 0.24
          #AIC: 789.5398
          #BIC: 986.2935
         
          
          
          
          #Best Model / BIC
          reg.fit.full<-regsubsets(response~.,data=training,nvmax=20)
          plot(reg.fit.full, scale = "bic")
          BIC_mod<-glm(response~ chk_acct+duration+credit_his+purpose+saving_acct+installment_rate+sex+other_install,family=binomial(link="logit"),training)
          summary(BIC_mod)
          BIC(BIC_mod) 
          rocplot(BIC_mod)
          BIC_prob<-ifelse(predict(BIC_mod,testing,type=c("response"))>1/6,1,0)
          BIC_miss_rate<-mean( BIC_prob!=testing$response )
          BIC_miss_rate
          #8 predictors
          #Mean residual deviance: 758.50
          #AUC: 0.8046
          #Misclassification rate: 0.21
          #AIC: 814.4994
          #BIC: 945.6686
          
          
          
          #Lasso regression
          X.train=model.matrix(response~.,training)[,-21]
          Y.train=training$response
          
          X.test=model.matrix(response~.,testing)[,-21]
          Y.test=testing$response
            
            #Coefficient vs lambda
            lasso.mod<-glmnet(x=X.train,y=Y.train,family = "binomial")
            plot(lasso.mod,xvar = "lambda")
          
            #Error bs lambda
            lasso.cv<-cv.glmnet(x=X.train,y=Y.train,alpha=1, nfolds=10)
            plot(lasso.cv)
            best_lam<-lasso.cv$lambda.min
            #0.004976994
            
            coef(lasso.mod, s= best_lam)
          
            
            lasso.mod.final.fit<-glm(response~.,family="binomial",training)
            lasso_prob<-ifelse(predict(lasso.mod,s=best_lam,newx = X.test)>1/6,1,0)
            lasso_miss_rate<-mean( lasso_prob!=testing$response )
            lasso_miss_rate
            #0.2155556
            
            AIC(lasso.mod.final.fit)
            BIC(lasso.mod.final.fit)
            rocplot(lasso.mod.final.fit)
            #19 predictors
            #Mean residual deviance: 703.14
            #AUC: 0.8436
            #Misclassification rate: 0.22
            #AIC: 799.1375
            #BIC: 1023.999
            
            
            #Residual Diagnostics
            par(mfrow=c(1,3),oma=c(0,0,2,0))
            plot(AIC_mod,1,main="AIC");plot(BIC_mod,1,main = "BIC");plot(lasso.mod.final.fit,1,main = "Lasso")
            
            par(mfrow=c(1,3),oma=c(0,0,2,0))
            plot(AIC_mod,2,main="AIC");plot(BIC_mod,2,main = "BIC");plot(lasso.mod.final.fit,2,main = "Lasso")
            
            par(mfrow=c(1,3),oma=c(0,0,2,0))
            plot(AIC_mod,3,main="AIC");plot(BIC_mod,3,main = "BIC");plot(lasso.mod.final.fit,3,main = "Lasso")
            
            par(mfrow=c(1,3),oma=c(0,0,2,0))
            plot(AIC_mod,4,main="AIC");plot(BIC_mod,4,main = "BIC");plot(lasso.mod.final.fit,4,main = "Lasso")
          
## Step 3:
          ## (a): Test out-of-sample performance of model from step 1
          ## (b): Report out-of-sample AUC and misclassification rate
              
                  
          log_mod<-glm(response~.,family=binomial(link="logit"),training)
          
          log_mod_prob<-ifelse(predict(log_mod,testing)>1/6,1,0)
          log_mod_miss_rate<-mean ( log_mod_prob != testing$response  )
          rocplot(log_mod)
          #Mean residual deviance: 701.95
          #AUC: 0.844
          #Misclassification rate: 0.22
          #AIC: 799.9512
          #BIC: 1029.497       
  
            
## Step 4:
          ## (a): Try different cut-off probabilities through grid search and find the "optimal" one
          
          
          # Define the searc grid from 0.01 to 0.99
          searchgrid = seq(0.01, 0.99, 0.01)
          # Result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd column
          # Stores the cost
          result = cbind(searchgrid, NA)
          # in the cost function, both r and pi are vectors, r=truth, pi=predicted
          # probability
          cost1 <- function(r, pi) {
            weight1 = 5
            weight0 = 1
            c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
            c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
            return(mean(weight1 * c1 + weight0 * c0))
          }
          log_mod<-glm(response~.,family=binomial(link="logit"),training)
          for (i in 1:length(searchgrid)) {
            pcut <- result[i, 1]
            # assign the cost to the 2nd col
            result[i, 2] <- cost1(training$response, predict(log_mod,testing))
          }
          colnames(result)<-c("Grid","Cost")
          result<-data.frame(result)
          g<-ggplot(data=result,aes(x=Grid,y=Cost))+geom_point(colour="blue")+ggtitle("Cost in Training Set")+theme(plot.title = element_text(hjust=0.5))
          g<-g+geom_point(x=result[result$Cost==min(result$Cost),][[1]]
                          ,
                          y=result[result$Cost==min(result$Cost),][[2]]
                          ,colour="red",cex=4)
          g
          
          #Optimal cut off = 0.11
          
          result
          
          
          
## Step 5: 
          ## (a): 5 fold Cross Validation on the 100% original dataset 
          ## (b): COmpare step 3 & 5 error rate
          ## (c): Specify right cost function. Plot ROC, misclassification rate (for credit data 5:1)
 
          pcut = result[result$Cost==min(result$Cost),][[1]]
          # Asymmetric cost
          cost1 <- function(r, pi) {
            weight1 = 5
            weight0 = 1
            c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
            c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
            return(mean(weight1 * c1 + weight0 * c0))
          }
          log_mod<-glm(response~.,family=binomial,German)
          cv.result<-cv.glm(data=German,glmfit = log_mod, K = 5, cost = cost1)
          cv.result$delta
          #0.5560 0.5544
          #CV seems to be an unreliable method to estimate model error in the case of classification treees
          
          
## Step 6:
          ## (a): Fit CART (use 5:1 loss)
          ## (b): Report out-of-sample AUC and misclassification rate
          
          German_rpart<-rpart(response~.,training,method = "class", parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)))
          fancyRpartPlot(German_rpart)
          plotcp(German_rpart)
                 
          # Predicted Class
          German_rpart_pred<-predict(German_rpart, testing, type = "class")
          chep<-table(testing$response, German_rpart_pred, dnn = c("Truth", "Predicted"))
          
          
              German_rpart_no_cost<-rpart(response~., data = training, method = "class", cp = 0.01,parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)))
              fancyRpartPlot(German_rpart_no_cost)
             
              # Probability of getting 1
              German_rpart_no_cost_prob<-predict(German_rpart_no_cost, testing, type = "prob")
              pred <- prediction(German_rpart_no_cost_prob[, 2], testing$response)
              perf <- performance(pred, "tpr", "fpr")
              plot(perf, colorize = TRUE)
              slot(performance(pred, "auc"), "y.values")[[1]]
              German_miss_rate_no_cost<- (chep[][2]+chep[][3])/nrow(testing)
              #AUC: 0.6700426
              #Misclassification rate: 0.43
              
              
              
              
## Step 7:
          ## (a): Compare CART and Best Logistic model (from AIC, BIC and lasso)
          
              #CART
                  #AUC: 0.6700
                  #Misclassification rate: 0.43
              
              #Best Model
                  #AUC: 0.8421
                  #Misclassification rate: 0.24
              
## Step 8:  
          ##(a): Generate new training and test dataset (80-20 , 90 - 10)
          ##(b): Compare CART and Best Logistic model (AIC, BIC and lasso)
          

          
          set.seed(4321)
          
          # 80-20
          index<-sample(nrow(German),0.8* nrow(German), replace = TRUE)
          training<-German[index,]
          testing<-German[-index,]
          
          
          #AIC
                  #AUC: 0.8421
                  #Misclassification: 0.24
                  null<-glm(response~1,family = binomial(link="logit"),training)
                  full<-glm(response~.,family = binomial(link="logit"),training)
                  AIC_obj<-stepAIC(glm(response~1,data=training), scope = list(lower=null, upper=full),direction = c("both"),trace = TRUE)
                  AIC_obj$anova
                  AIC_mod<-glm(response ~ chk_acct + credit_his + duration + purpose + other_debtor + 
                                 saving_acct + other_install + foreign + present_emp + amount + 
                                 property + housing + installment_rate + sex + present_resid, family = binomial(link="logit"),training)
                  AIC(AIC_mod)
                  rocplot(AIC_mod)
                  AIC_prob<-ifelse(predict(AIC_mod,testing,type=c("response"))>pcut,1,0)
                  AIC_miss_rate<-mean( AIC_prob!=testing$response )
                  AIC_miss_rate
          
                  
                  
                  
          #CART
                  #AUC: 0.67
                  #Misclassification: 0.43
                  German_rpart_no_cost<-rpart(response~., data = training, method = "class", cp = 5e-04)
                  fancyRpartPlot(German_rpart_no_cost)
                  
                  plotcp(German_rpart_no_cost)
                  # Probability of getting 1
                  German_rpart_no_cost_prob<-predict(German_rpart_no_cost, testing, type = "prob")
                  pred <- prediction(German_rpart_no_cost_prob[, 2], testing$response)
                  perf <- performance(pred, "tpr", "fpr")
                  plot(perf, colorize = TRUE)
                  slot(performance(pred, "auc"), "y.values")[[1]]
                  German_miss_rate_no_cost<- (chep[][2]+chep[][3])/nrow(testing)
                  German_miss_rate_no_cost
                  
                  
                  
                  
                  
          # 90-10
          index<-sample(nrow(German),0.9* nrow(German), replace = TRUE)
          training<-German[index,]
          testing<-German[-index,]
                    
          
          
          #AIC
                  #AUC: 0.8542
                  #Misclassification: 0.41
                  null<-glm(response~1,family = binomial(link="logit"),training)
                  full<-glm(response~.,family = binomial(link="logit"),training)
                  AIC_obj<-stepAIC(glm(response~1,data=training), scope = list(lower=null, upper=full),direction = c("both"),trace = TRUE)
                  AIC_obj$anova
                  AIC_mod<-glm(response ~ chk_acct + credit_his + duration + purpose + other_debtor + 
                         saving_acct + other_install + foreign + present_emp + amount + 
                         property + housing + installment_rate + sex + present_resid, family = binomial(link="logit"),training)
                  AIC(AIC_mod)
                  rocplot(AIC_mod)
                  AIC_prob<-ifelse(predict(AIC_mod,testing,type=c("response"))>pcut,1,0)
                  AIC_miss_rate<-mean( AIC_prob!=testing$response )
                  AIC_miss_rate
          
          
          
          
          #CART
                  #AUC: 0.71
                  #Misclassification: 0.48
                  German_rpart_no_cost<-rpart(response~., data = training, method = "class", cp = 5e-04)
                  # Probability of getting 1
                  German_rpart_no_cost_prob<-predict(German_rpart_no_cost, testing, type = "prob")
                  pred <- prediction(German_rpart_no_cost_prob[, 2], testing$response)
                  perf <- performance(pred, "tpr", "fpr")
                  plot(perf, colorize = TRUE)
                  slot(performance(pred, "auc"), "y.values")[[1]]
                  German_miss_rate_no_cost<- (chep[][2]+chep[][3])/nrow(testing)
                  German_miss_rate_no_cost
          