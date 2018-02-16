library("ggplot2")
x<-1:50
y<-dpois(x,lambda = 6.7)
dat<-data.frame(x,y)
g<-ggplot(data=dat,aes(x,y))+scale_x_continuous(breaks = c(1:50))+geom_vline(xintercept = 6.7)+geom_line()+geom_point()
g

p_5<-dpois(5,6.7)  #0.1384904
cdf_5<-ppois(5,6.7) #0.3406494

1-punif(37,30,40) #0.3
punif(32,30,40) #0.2
punif(38,30,40) - punif(34,30,40) #0.4

-0.5998593   2.39989
100       190
22.57  +  49.18
27.43     0.82

x<-seq(from = 0, to = 8,by=0.5)
y<-dexp(x,rate = 1/5)
plot(x,y)


pexp(6,rate = 1/5) #0.6988058
pexp(5,rate = 1/5) - pexp(3,rate = 1/5) #0.1809322


dbinom(25,400,.07) #0.06867971
pbinom(24,400,.07) #0.2511457
pbinom(25,400,.07) - pbinom(20,400,.07) #00.2541306



#Solution 6
x<-rpois(100,3)
y<-x^2
dat<-data.frame(x,y)
#a
g<-ggplot(data = dat,aes(x=dat$y))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Y (1st part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Y")+ylab("Frequency")
g



dt<-data.frame(y_bar=0,sd=0)  #Initializing an empty data frame
for (i in 1:1000) {  #looping 1000 times to store y_bar in dt
x<-rpois(100,3)
y<-x^2
y_bar<-mean(y)
sd<-sd(y)
dt<-rbind(dt,cbind(y_bar,sd))
}
dt<-data.frame(dt[2:1001,])
#b
g<-ggplot(data = dt,aes(x=dt$y))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Y_bar (1st part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Y_bar")+ylab("Frequency")
g


u_bar<-mean(dt$y_bar)
dt$z<-with(dt, (dt$y_bar - u_bar)/(dt$sd/10))
mean_z<-mean(dt$z)
sd_z<-sd(dt$z)
#c
g<-ggplot(data = dt,aes(x=dt$z))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Z & Fitted normal curve (1st part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Z")+ylab("Frequency")
g<-g+stat_function(fun = function(x,mean_z,sd_z){
dnorm(x=x,mean_z,sd_z)*1000},
args = c(mean = mean_z, sd = sd_z)
,size = 1.5,col = "blue")
g


#d
x<-rexp(100,1/3)
y<-x^2
dat<-data.frame(x,y)
g<-ggplot(data = dat,aes(x=dat$y))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Y (2nd part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Y")+ylab("Frequency")
g



dt<-data.frame(y_bar=0,sd=0)  #Initializing an empty data frame
for (i in 1:1000) {  #looping 1000 times to store y_bar in dt
  x<-rpois(100,3)
  y<-x^2
  y_bar<-mean(y)
  sd<-sd(y)
  dt<-rbind(dt,cbind(y_bar,sd))
}
dt<-data.frame(dt[2:1001,])
g<-ggplot(data = dt,aes(x=dt$y))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Y_bar (2nd part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Y_bar")+ylab("Frequency")
g


u_bar<-mean(dt$y_bar)
dt$z<-with(dt, (dt$y_bar - u_bar)/(dt$sd/10))
mean_z<-mean(dt$z)
sd_z<-sd(dt$z)
g<-ggplot(data = dt,aes(x=dt$z))+geom_histogram(binwidth = 1,col = "red")
g<-g+ggtitle("Distribution of Z & Fitted normal curve (2nd part)")+theme(plot.title = element_text(hjust=0.5))+xlab("Z")+ylab("Frequency")
g<-g+stat_function(fun = function(x,mean_z,sd_z){
  dnorm(x=x,mean_z,sd_z)*1000},
  args = c(mean = mean_z, sd = sd_z)
  ,size = 1.5,col = "blue")
g


