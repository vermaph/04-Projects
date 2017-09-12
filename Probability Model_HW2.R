library("ggplot2")
#############################Solution 18#############################
#(a)
#Given: X~N(3,16) 
#To solve: P(X<7)
x<-seq(-12,18,length = 1000)
y<-dnorm(x,mean=3,sd=sqrt(16))
dat<-data.frame(x,y)
g<-ggplot(data=dat,aes(x=x,y=y))+geom_line()+ggtitle("Normal Distribution")+theme(plot.title = element_text(hjust = 0.5))
g<-g+xlab("X")+ylab("pmf(x)")+scale_x_continuous(breaks = c(-12,-6,0,6,7,12))+geom_vline(xintercept = 7)
g<-g+geom_ribbon(data = dat[dat$x<7,],aes(x=x,ymax=y),ymin=0,fill="red", alpha=0.5)
g
pnorm((7-3)/(4),lower.tail = TRUE)
#(b)
#(c)
#(d)
#(e)

