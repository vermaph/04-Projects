########################

##    Data Mining
##    Assignment 3
##    PIYUSH VERMA
##    2/05/2018

########################




####### Problem 1 #######


####### (a)
x1<-c()
x2<-c()

for(i in 1:500){
  x1[i]<-runif(1,0,1)
  x2[i]<-ifelse(i %% 2 == 0,0,1)
}

n<- 1.2 + 5*x1 - 0.4 * x2
p<- ilogit(n)   ### response = inverse logit

data<-data.frame(cbind(p,x1,x2))
colnames(data)<-c("prob","x1","x2")


## Probit
probit<-glm(prob~.,family = binomial(link="probit"), data)
p_probit<-predict(probit,data)
p_probit<-exp(p_probit)/(1+exp(p_probit))


## Logit
logit<-glm(prob~.,family = binomial(link="logit"), data)
p_logit<-predict(logit,data)
p_logit<-exp(p_logit)/(1+exp(p_logit))





####### (b)
### response = inverse probit




####### Problem 2 #######


german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

# orginal response coding 1= good, 2 = bad we need 0 = good, 1 = bad
german_credit$response = german_credit$response - 1








####### Problem 3 #######

bank.data <- bankruptcy
colnames(bank.data)

head(bank.data)

str(bank.data)

summary(bank.data)

#EDA

hist(bank.data$R1)

hist(bank.data$R2)

hist(bank.data$R3)

hist(bank.data$R4)

hist(bank.data$R5)

hist(bank.data$R6)

hist(bank.data$R7)

hist(bank.data$R8)

hist(bank.data$R9)

hist(bank.data$R10)



pairs(bank.data[4:13])



#test-train data

subset <- sample(nrow(bank.data), nrow(bank.data) * 0.8)

bank.train = bank.data[subset, ]

bank.test = bank.data[-subset, ]



#modeling train data

bank.glm0 <- glm(DLRSN ~ . - CUSIP -FYEAR, family = binomial, bank.train)

summary(bank.glm0)



#model selection

bank.glm.1b <- step(bank.glm0, k = log(nrow(bank.train))) #BIC

bank.glm.1a <- step(bank.glm0, k = 2) #AIC



hist(predict(bank.glm.1b))

hist(predict(bank.glm.1b, type = "response"))



prob.glm1b.insample <- predict(bank.glm.1b, type = "response")

predicted.glm1b.insample <- prob.glm1b.insample > 1/16

predicted.glm1b.insample <- as.numeric(predicted.glm1b.insample)

table(bank.train$DLRSN, predicted.glm1b.insample, dnn = c("Actual", "Predicted"))

mean(ifelse(bank.train$DLRSN != predicted.glm1b.insample, 1, 0))



#ROC Curve, AUC and misclassification- insample

library(ROCR)

pred1 <- prediction(prob.glm1b.insample, bank.train$DLRSN)

perf1 <- performance(pred1, "tpr", "fpr")

plot(perf1, colorize = TRUE, main = "In sample model ROC")

perf1auc <- performance(pred1, "auc")

perf1auc@y.values[[1]]

mean(ifelse(bank.train$DLRSN != predicted.glm1b.insample, 1, 0))



#model performance-test data

prob.glm1b.outsample <- predict(bank.glm.1b, bank.test, type = "response")

predicted.glm1b.outsample <- prob.glm1b.outsample > 1/16

predicted.glm1b.outsample <- as.numeric(predicted.glm1b.outsample)

table(bank.test$DLRSN, predicted.glm1b.outsample, dnn = c("Actual", "Predicted"))

mean(ifelse(bank.test$DLRSN != predicted.glm1b.outsample, 1, 0))



#ROC Curve

library(ROCR)

pred <- prediction(prob.glm1b.outsample, bank.test$DLRSN)

perf <- performance(pred, "tpr", "fpr")

plot(perf, colorize = TRUE, main = "Out of sample model ROC")

perfauc <- performance(pred, "auc")

perfauc@y.values[[1]]





#Asymmetric misclassification rate

pcut = 1/16

# Asymmetric cost

cost <- function(r, pi) {
  
  weight1 = 2
  
  weight0 = 1
  
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  
  return(mean(weight1 * c1 + weight0 * c0))
  
} 



library(boot)

bank.glm3 <- glm(DLRSN ~ R2+R3+R6+R7+R8+R9+R10, family = binomial, bank.data)

cv.result = cv.glm(bank.data, bank.glm3, cost, 10)

cv.result$delta #raw pred error, cost adjusted error

