
#My code for Comp Stats. Project

setwd("/Users/victoragboli/Documents/Fall 22 Semester/Computational Statistics/Projects/Comp Stats Project")
data1 = read.table("Abortion Data.txt")

#check data type
str(data1)

#2015 vs. 2016 Analysis
pre = data1[,1]
post = data1[,2]

#histogram
par(mfrow=c(2,2))
hist(pre,50,main="Abortions in 2015",col="blue")
hist(post,50,main="Abortions in 2016",col="red")

#log transform
pre = log(pre)
post = log(post)

mean(pre)
mean(post)

#histogram
hist(pre,50,main="Log Abortions in 2015",col="blue")
hist(post,50,main="Log Abortions in 2016",col="red")

#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(pre, mean=mu1,sd=sig1))*prod(dnorm(post,mean=mu2,sd=sig2))
}

#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,8.767,8.767)*dnorm(mu2,8.768,8.768)*dexp(sig1,rate=1/8.767)*dexp(sig2,rate=1/8.767)
}

#posterior
Posterior=function(th){Prior(th)*like(th)}

#starting
mu1=8.767; sig1=8.767; mu2=8.768; sig2=8.768
th0=c(mu1,sig1,mu2,sig2)
nit=1000000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.003)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}

#edit(results)
#getting the trace-plot
par(mfrow=c(4,1))
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])


#removing the burns from the traceplots
res=results[2.7e+05:1e+06,]
par(mfrow=c(4,1))
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])

mu1s=res[,1]
sig1s=res[,2]
mu2s=res[,3]
sig2s=res[,4]

par(mfrow=c(2,1))
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)



#2017 vs. 2018 Analysis

#clear data before running
pre = data1[,3]
post = data1[,4]

#histogram
par(mfrow=c(2,2))
hist(pre,50,main="Abortions in 2017",col="blue")
hist(post,50,main="Abortions in 2018",col="red")

#log transform
pre = log(pre)
post = log(post)

#histogram of log
hist(pre,50,main="Log Abortions in 2017",col="blue")
hist(post,50,main="Log Abortions in 2018",col="red")

mean(pre)
mean(post)

#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(pre, mean=mu1,sd=sig1))*prod(dnorm(post,mean=mu2,sd=sig2))
}

#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,8.762,8.762)*dnorm(mu2,8.757,8.757)*dexp(sig1,rate=1/8.762)*dexp(sig2,rate=1/8.757)
}

#posterior
Posterior=function(th){Prior(th)*like(th)}

#starting
mu1=8.762; sig1=8.762; mu2=8.757; sig2=8.757
th0=c(mu1,sig1,mu2,sig2)
nit=10000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.1)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}


par(mfrow=c(4,1))
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])

res=results[1001:10000,]
par(mfrow=c(4,1))
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])

mu1s=res[,1]
sig1s=res[,2]
mu2s=res[,3]
sig2s=res[,4]
par(mfrow=c(2,1))
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)


#2019 vs. 2020 Analysis
#remember to clear data before running
pre=data1[,5]
post=data1[,6]

#histogram
par(mfrow=c(2,2))
hist(pre,50,main="Abortions in 2019",col="blue")
hist(post,50,main="Abortions in 2020",col="red")

#log-transform
pre=log(pre)
post=log(post)

#log histogram
hist(pre,50,main="Log Abortions in 2019",col="blue")
hist(post,50,main="Log Abortions in 2020",col="red")

#mean
mean(pre)
mean(post)

#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(pre, mean=mu1,sd=sig1))*prod(dnorm(post,mean=mu2,sd=sig2))
}

#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,8.749,8.749)*dnorm(mu2,8.712,8.712)*dexp(sig1,rate=1/8.749)*dexp(sig2,rate=1/8.712)
}

#posterior
Posterior=function(th){Prior(th)*like(th)}

#starting
mu1=8.749; sig1=8.749; mu2=8.712; sig2=8.712
th0=c(mu1,sig1,mu2,sig2)
nit=10000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.2)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}

#edit(results)
#traceplot
par(mfrow=c(4,1))
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])

#removing burnout
res=results[301:10000,]
par(mfrow=c(4,1))
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])

#prob
mu1s=res[,1]
sig1s=res[,2]
mu2s=res[,3]
sig2s=res[,4]
par(mfrow=c(2,1))
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)

#visualising of the data
par(mfrow=c(3,2))
plot(data1[,1], type = "l", main = "Abortions in 2015")
plot(data1[,2], type = "l", main = "Abortions in 2016")
plot(data1[,3], type = "l", main = "Abortions in 2017")
plot(data1[,4], type = "l", main = "Abortions in 2018")
plot(data1[,5], type = "l", main = "Abortions in 2019")
plot(data1[,6], type = "l", main = "Abortions in 2020")


