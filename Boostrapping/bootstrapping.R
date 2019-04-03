####Bootstraping in R####
library(ggplot2)
set.seed(45056)

#### Simulated Normal Data example ####
#This example will show how to use the bootstrap to create confidence
#intervals around parameters.

#Our goal is to show how a bootstrapped 95% confidence interval can replace the 
#95% t-confidence interval for the mean that we've all learned before

#Start with some simulated data
x = rnorm(250,50,10)

ggplot()+
  geom_histogram(aes(x))

#how do we typically find error bounds for a parameter?
#We use confidence interval
t.test(x,conf.level = 0.95)$conf.int

#Weaknesses of this approach? 
#We need to know that the interval we are using is valid for the
#distribution of the parameter. In this case, we know mu is normally
#distributed, so we can use a 95% t confidence interval

#How can we do this using the bootstrap?

#First, how many bootstrapped samples do we want? 
#Typically 1,000 will work, but feel free to shoot higher
boot_n = 1000

#Also need to know our sample size from the original data
n = length(x)

#For performance reasons, it is fastest to create a blank data
#vector before taking samples.
boot_mean = rep(NA,boot_n)

#Now take 1000 samples of size n WITH REPLACEMENT from the original data,
#and calculate the mean of each.
for(i in 1:boot_n){
  boot_x = sample(x,size = n,replace = TRUE)
  boot_mean[i] = mean(boot_x)
}

#Now we have obtained a simulated samping distribution for the mean of x
#Let's take a look
ggplot()+
  geom_histogram(aes(boot_mean))

mean(boot_mean)
#Looks like it's centered around 50 and has pretty small variance.
#But how can we find the confidence interval?

#Instead of using a formula, we can simply take the quantiles
#of interest right from the simulated bootstrap sample statistics.

#For a 95% interval, we want the 2.5th and 97.5th percentiles
quantile(x = boot_mean, probs = c(.025,.975))

t.test(x,conf.level = 0.95)$conf.int
#When we use the bootstrap, there are going to be random deviations.
#This is why we pick a large number of bootstrapped samples (boot_n)

#### Real data example ####
#Now that we've all seen how to obtain bootstrapping confidence intervals,
#let's take a look at some real data

#Our goal here is to work with data of an unknown distribution and 
#find the sampling distribution of a couple statistics of interest.

#Here is some data I gathered on the windspeed at Kenston High School
#From december 2016 to November 2017
windspeed = read.csv("http://garretrc.github.io/host/windspeed.txt")

speed = windspeed$Wind.Speed

ggplot(windspeed)+
  geom_histogram(aes(speed))

#What's the average windspeed we can expect on any given day?
#Additionally what kind of speed can we expect on the windiest days of the year?
#Windspeed does not seem to follow a normal distribution.
#t-confidence interval is likely not appropriate, so lets use the bootstrap
boot_n = 10000
n = length(speed)

boot_median = rep(NA,boot_n)
boot_95th = rep(NA,boot_n)

#Now obtain the samples
for(i in 1:boot_n){
  boot_speed = sample(speed,size = n,replace = TRUE)
  
  #calculate the wanted statistics
  boot_median[i] = median(boot_speed)
  boot_95th[i] = quantile(boot_speed,.95)
}

#What is our distribution and 95% confidence interval around the median?
ggplot()+
  geom_histogram(aes(boot_median),binwidth = .125)

quantile(boot_median,c(.025,.975))

#What is our distribution and 95% confidence interval around the 95th percentile?
ggplot()+
  geom_histogram(aes(boot_95th),binwidth= .25)

quantile(boot_95th,c(.025,.975))

#This distribution can be simulated by a Gamma, lets see how good the fit is!
xbar = mean(speed)
sse = sum((speed-xbar)^2)

beta = (1/(n*xbar))*sse
alpha = xbar/beta

gamma_sim = rgamma(n,alpha,beta)

sim = ggplot()+
  geom_histogram(aes(gamma_sim))+
  xlim(0,35)

real = ggplot()+
  geom_histogram(aes(speed))+
  xlim(0,35)

library(gridExtra)
grid.arrange(sim,real,ncol=1)

#Hm, seems like this simulated distribution is not quite a perfect fit.
#Lets do what we did before but make error intervals around the 
#median and 95th percentiles

boot_n = 10000
n = length(speed)

error_median = rep(NA,boot_n)
error_95th = rep(NA,boot_n)

#Now obtain the samples
for(i in 1:boot_n){
  boot_speed = sample(speed, size = n,replace = TRUE)
  boot_sim = sample(gamma_sim, size = n, replace = TRUE)
  
  #calculate the wanted statistics
  error_median[i] = median(boot_sim) - median(boot_speed) 
  error_95th[i] = quantile(boot_sim,.95) - quantile(boot_speed,.95) 
}

#How far is the center of the gamma from the true median?
ggplot()+
  geom_histogram(aes(error_median))

quantile(error_median,c(.025,.975))

#How far is the 95th percentile of the gamma from the true 95th percentile?
ggplot()+
  geom_histogram(aes(error_95th))

quantile(error_95th,c(.025,.975))
