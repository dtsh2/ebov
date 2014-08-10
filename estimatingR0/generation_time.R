data<-read.csv("incubation.csv",header=F)
data<-t(data)
colnames(data)<-data[1,1:7]
labels<-colnames(data)
data<-data[2:6,2:7]
dat<-as.numeric(data[,])
dat<-as.data.frame(matrix(dat,ncol=6))
colnames(dat)<-labels[2:7]

# for 1 ...
sd1<-(7.3-6.3)/1.96
hist(rnorm(56,mean=6.3,sd=sd1))
sd2<-(8-6.2)/4
hist(rnorm(56,mean=6.3,sd=sd2))
sd3<-(21-12)/4
hist(rnorm(56,mean=6.3,sd=sd3))
sd4<-(21-6.3)/4
hist(rnorm(56,mean=6.3,sd=sd4))

f1<-(rnorm(56,mean=6.3,sd=sd1))
f2<-(rnorm(56,mean=6.3,sd=sd2))
f3<-(rnorm(56,mean=6.3,sd=sd3))
f4<-(rnorm(56,mean=6.3,sd=sd4))

res<-c(f1,f2,f3,f4)
hist(res)
hist(res[res >= 0])
res1<-(res[res >= 0])
hist(res1,breaks=500)
plot(density(res1))
plot(ecdf(res1))
z.norm<-(res1-mean(res1))/sd(res1) ## standardized data
qqnorm(z.norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line
res2<-as.matrix(res1,nrow=1)
library(fitdistrplus)
## Fit a log-normal distribution to the 50 random data set
# flnorm <- apply(res2, 2,  fitdist, "lnorm")
lnfit<-fitdist(res1,distr="lnorm")
## Plot the results 
lnfit$aic
summary(lnfit)
#plot(flnorm[[1]])
plot(lnfit)

gamfit<-fitdist(res1,distr="gamma")
## Plot the results 
gamfit$aic
summary(gamfit)
plot(gamfit)

nfit<-fitdist(res1,distr="norm")
## Plot the results 
nfit$aic
summary(nfit)
plot(nfit)

samples<-(rgamma(100,shape=gamfit$estimate[1],rate=gamfit$estimate[2]))
hist(samples)

