setwd("C:/STONY/Practice/R (No.6)")
install.packages("quantmod")
library(quantmod)
library(ggplot2)
library(dplyr)

####### 1 #######
symbols=c("MSFT", "IBM")		#Assume we retrieve data of two stocks
getSymbols(symbols,from="2016-06-24",to="2018-06-24",periodicity="weekly",
           return.class="ts")
mydat<-cbind(MSFT[,6], IBM[,6])
colnames(mydat)<-c("MSFT","IBM")

#save the data back to your computer
write.csv(mydat, file="stock_data.csv")

### read the data ###
mydata <- read.csv("stock_data.csv",header=T)
str(mydata)

### calculate the MSFT return  ### calculate the IBM return
x<-diff(log(mydata$MSFT))
y<-diff(log(mydata$IBM))
return.x <- c()
return.y <- c()
return.x[2:105] <- x
return.y[2:105] <- y
return.x[1] <- NA
return.y[1] <- NA
# generate one column "date" which represents the date
install.packages("lubridate")
library(lubridate)
Date <- c()
for (i in 1:105){
  Date[i] <- ymd(20160617) + days(7 %*% i)
}

mydata$date <- as.Date(Date,origin="1970-01-01")

## read the factor data and merge factor and stock returns together
factor <- read.csv("Factors.csv",header=T)
factor$Date <- factor$Date %>% as.numeric()
factor$Date <- as.Date(as.character(factor$Date), "%Y%m%d")
factor <- filter(factor,Date >= "2016-06-24"& Date <= "2018-06-24")
new_dat <-cbind(return.x,return.y,factor)
colnames(new_dat) <- c("M_rtn","I_rtn","date","RMRF","SMB","HML",'RF')
new_dat[,c(1,2)][1,] <- c(0,0)

head(new_dat)

############### 2 ##############
### histogram of stock's returns ###
hist(return.x, prob=T, col="red", ylim=c(0,40))

hist(return.y, prob=T, col="red", ylim=c(0,40))


### time series of stock's returns ###  
par(mfrow=c(2,1))   #combine multiple plots into one graph
plot(new_dat$M_rtn,type="l",  col="red", xlab="time", ylab="closing values", main="Stock MSFT")  #type="l' refers to a "line" plot
plot(new_dat$I_rtn,type="l", col="blue", xlab="time", ylab="closing values", main="Stock IBM")  


####### 3 #######
###  pairwise scatter plot  ###
plot(new_dat, main="Scatter plot")

### correlation function
cor <- cor(new_dat[,c(5,6,4)]) %>% as.matrix()
install.packages("corrplot")
library(corrplot)
corrplot(cor, method="circle") # the darker color is, the greater the correlation will be
corrplot(cor, method="number")

####### 4 #######
### regression model
excs_rtn1 <- new_dat[,1] - new_dat$RF
model1 <- {excs_rtn1 ~ RMRF+SMB+HML}
fit1 <- lm(model1,data=new_dat)

excs_rtn2 <- new_dat[,2] - new_dat$RF
model2 <- {excs_rtn2 ~ RMRF+SMB+HML}
fit2 <- lm(model2,data=new_dat)

####### 5 #######
coefficients(fit1) #model coefficients
confint(fit1, level=0.95) # CIs for model parameters
anova(fit1)
## the ANOVA table shows that RMRF and SMB are significant in this regression on MSFT stock 

coefficients(fit2) #model coefficients
confint(fit2, level=0.95) # CIs for model parameters
anova(fit2)
# the ANOVA table shows that RMRF and HML are significant in this regression on IBM stock 


####### 6 #######
step(fit1)
## find out the best linear model for MSFT
M_excs <- lm(formula = excs_rtn1 ~ RMRF + SMB, data = new_dat)
summary(M_excs)
#Adjusted R-squared:  0.4748 , RMRF is *** ,SMB ** significant for MSFT
#This means the trend of the stock follows the market trend. Here SMB is less than 0, this shows 
#Microfost's excess earning is affected by large-cap stocks.
#the value of MSFT larger, the more excess earning MSFT will get

step(fit2)
## find out the best linear model for IBM
I_excs <- lm(formula = excs_rtn2 ~ RMRF + SMB + HML, data = new_dat)
summary(I_excs)
#Adjusted R-squared:  0.3653, RMRF is *** ,HML * significant for MSFT
#Here SMB is not significant for Pvalue is larger  than 0.1
#the value of MSFT larger, the more excess earning MSFT will get


##### 7 #######
### market model
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

myts.x<-ts(new_dat$M_rtn) # MSFT
plot(myts.x)


myts.y<-ts(new_dat$I_rtn) #IBM
plot(myts.y)
#Augmented Dickey Fuller test
adf.test(myts.x)   #alternative hypothesis: stationary
adf.test(myts.y)   # stationary choose d = 0

#ACF and PACF
par(mfrow=c(2,1))   #combine multiple plots into one graph for MSFT 
acf(myts.x,main="ACF of Value Difference in PL")# q=2
pacf(myts.x,main="PACF of Value Difference in PL")# p=2


par(mfrow=c(2,1))   #combine multiple plots into one graph for IBM
acf(myts.y,main="ACF of Value Difference in P")  # q=5
pacf(myts.y,main="PACF of Value Difference in IBM") # p=5

#fit a MA(1) model FOR MSFT
myts.fit.x<-arima(myts.x,order=c(2,0,2))
#residual diagnostics
tsdiag(myts.fit.x)


#fit a MA(1) model FOR IBM
myts.fit.y<-arima(myts.y,order=c(6,0,12))
#residual diagnostics
tsdiag(myts.fit.y)

#MA Prediction
myts.pred<-predict(myts.fit.x, n.ahead=12)
plot(myts.x)
lines(myts.pred$pred,col="red")
lines(myts.pred$pred+2*myts.pred$se,col="red",lty=3)
lines(myts.pred$pred-2*myts.pred$se,col="red",lty=3)

# we can see 3 factor model outperforms the market model beacause it has more factors to predict
# the plot of MA model has abnormal predction which should be considered seriously
# This means 3 facotr model fits the data better.


####### 8 ######
### check assumptions ###

### MSFT
par(mfrow=c(2,2))
plot(M_excs)
### IBM
par(mfrow=c(2,2))
plot(I_excs)

## plot the residual diagnosis
install.packages("car")
library(car)
# test of independence
durbinWatsonTest(M_excs)  #p-value = 0.03  independent in the 99% confidence interval
durbinWatsonTest(I_excs)  #p-value = 0.1  independent in the 90%,95%,99% confidence interval
# test of normality
shapiro.test(M_excs$residuals) #p-value = 0.06   normal in the 99% confidence interval
shapiro.test(I_excs$residuals) #p-value = 4e-05  abnormal in the 90%,95%,99% confidence interval
# test of heterscedasticity 
library(lmtest)
bptest(M_excs)  #p-value = 0.8071  normal
bptest(I_excs)  #p-value = 0.7868   normal
