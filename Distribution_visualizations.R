#Binomial Distribution Visualization

install.packages("manipulate")
library("manipulate")
manipulate(
{
hist(rbinom(trials,size = trials,p=probability)
     ,xlim = c(0,trials)
     ,xlab="Number of trials"
     ,ylab="Probability"
     ,probability = TRUE
     ,main = paste("Binomial distribution of ",trials,"trials with success probability = ", round(probability,2))
     ,col = "blue"
     )
p<-dbinom(0:trials,size = trials,p=probability)
x<-0:trials
arr<-p*x
exp<-sum(arr)

legend("topright",legend = parse(text = sprintf('paste(mean,\' = %s\')',round(exp,2))))
}
,trials=slider(1,100)
,probability=slider(0,1)
)
