### 1 ###
### 
NWF <- read.table("C:/STONY BROOK/Practice/R _boostrap(No.2)/norwegianfire.txt", header = T, stringsAsFactors = T)
str(NWF)
head(NWF)
nwf <-subset(NWF,NWF[,2]== 88)
str(nwf)
summary(nwf)

hist(nwf[,1],xlab="range",ylab="num",border="blue", col="green",xlim = c(400,470000),ylim = c(1,20),breaks = 100000)

### 2 ### 
boot_n = 1000
n <- dim(nwf)[1]
boot_median = rep(NA,boot_n)
boot_95th = rep(NA,boot_n)
#Now obtain the samples
for(i in 1:boot_n){
  boot = sample(rownames(nwf),size = n,replace = TRUE)
  aa <- a[boot,]
  #calculate the wanted statistics
  boot_median[i] = median(aa$X520)
  boot_95th[i] = quantile(aa$X520,.95)
}
plot(density(boot_95th))
ggplot()+
  geom_histogram(aes(boot_95th),bins=30)

#### 3 ### bias and MSE
theta.b <- quantile(nwf$X520,.95)
theta.hat <- mean(boot_95th)
bias <- theta.b - theta.hat
bias

MSE <- var(boot_95th)+bias^2
MSE

### 4 ### 95th confidenceinterval for 95th quantile
# quantile(x = boot_95th,c(.025,.975))
t.test(boot_95th,conf.level = 0.95)$conf.int

### Jackknife resampling ###
### 5 ### get the result of 95th quantile of the jackknife resamppling 
jack_n = 827
jack_median = rep(NA,jack_n)
jack_95th = rep(NA,jack_n)
#Now obtain the samples
for(i in 1:jack_n){
  temp_x = rownames(nwf)[-i]
  jack <- nwf[temp_x,]
  #calculate the wanted statistics
  jack_median[i] = median(jack$X520)
  jack_95th[i] = quantile(jack$X520,.95)
}
plot(density(jack_95th))
ggplot()+
  geom_histogram(aes(jack_95th),bins=30)
### 6 ### bias and MSE
theta.b <- quantile(nwf$X520,.95)
theta.jack <- mean(jack_95th)
bias1 <- theta.b - theta.jack
bias1
MSE1 <- var(boot_95th)+bias^2
MSE1
#mean((theta.b - theta.jack)^2)

### 7 ### 95th confidence interval
quantile(x = jack_95th,c(.025,.975))
t.test(jack_95th,conf.level = 0.95)$conf.int

### 8 ### plot side by side confidence intervals 
require(plotrix)
# Plot the confidence interval
par(mfrow=c(1,2))
n8 <- sapply(boot_95th,length)
ciw8 <- qt(0.95, n8) * MSE / sqrt(n8)
plotCI(x=mean(boot_95th), uiw=ciw8, col="black", barcol="blue")
n9 <- sapply(jack_95th,length)
ciw9 <- qt(0.95, n8) * MSE / sqrt(n9)
plotCI(x=mean(jack_95th), uiw=ciw9, col="red", barcol="black")
### 9 ### plot side by side distribution of sampling 
par(mfrow=c(1,2))
hist(boot_95th)
hist(jack_95th)

