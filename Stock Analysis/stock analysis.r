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
data <- read.csv("stock_data.csv",header=T)
str(data)

### calculate the MSFT return
return1 <- c()
for (i in 2:105){
  if(i==1){
    return1[i] <- data[,2][1] - data[,2][1]
  }
  else{
    return1[i] <- (data[,2][i] - data[,2][i-1])/data[,2][i]
  }
}

### calculate the IBM return
return2 <- c()
for (i in 2:105){
  if(i==1){
    return2[1] <- data[,3][1] - data[,3][1]
  }
  else{
    return2[i] <- (data[,3][i] - data[,3][i-1])/data[,3][i]
  }
}

# generate one column "date" which represents the date
install.packages("lubridate")
library(lubridate)
Date <- list()
for (i in 1:105){
  Date[[i]] <- ymd(20160617) + days(7 %*% i)
}

data$date <- unlist(Date) %>% as.Date()
## read the factor data and merge factor and stock returns together
factor <- read.csv("Factors.csv",header=T)
factor$Date <- factor$Date %>% as.numeric()
factor$Date <- as.Date(as.character(factor$Date), "%Y%m%d")
factor <- filter(factor,Date >= "2016-06-24"& Date <= "2018-06-24")
new_dat <-cbind(return1,return2,factor)
colnames(new_dat) <- c("M_rtn","I_rtn","date","RMRF","SMB","HML",'RF')
new_dat[,c(1,2)][1,] <- c(0,0)


### histogram of stock's returns ###
ggplot(new_dat, aes(x = I_rtn)) +
  geom_histogram(fill = "lightblue", colour = "black",bins = 20) +
  labs(title = "Histogram of stock returns Of IBM")

ggplot(new_dat, aes(x = M_rtn)) +
  geom_histogram(fill = "lightblue", colour = "black",bins = 20) +
  labs(title = "Histogram of stock returns Of MSFT")

### time series of stock's returns ###  
library(xts)
hh <- xts(new_dat$I_rtn, as.Date(new_dat$date, format='%Y/%m/%d'))  
win.graph(width = 9.5,height = 4.5,pointsize = 8)
plot(hh,type = 'l',main=' ') 
title("Timeseries of stock returns of IBM")

kk <- xts(new_dat$M_rtn, as.Date(new_dat$date, format='%Y/%m/%d'))  
win.graph(width = 9.5,height = 4.5,pointsize = 8)
plot(kk,type = 'l',main=' ') 
title("Timeseries of stock returns of MSFT")

head(new_dat)
####### 3 #######
###  pairwise scatter plot  ###
plot(I_rtn~RMRF,data=new_dat)
plot(I_rtn~SMB,data=new_dat)
plot(I_rtn~HML,data=new_dat)
plot(M_rtn~RMRF,data=new_dat)
plot(M_rtn~SMB,data=new_dat)
plot(M_rtn~HML,data=new_dat)
# pairwise 
# among the 3 factors
pairs(new_dat[,c(5,6,4)])
# among MSFT return and 3 factor
pairs(new_dat[,c(1,5,6,4)])
# among IBM return and 3 factor
pairs(new_dat[,c(2,5,6,4)])


### correlation function
cor <- cor(new_dat[,c(5,6,4)])
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
summary(fit1)
anova(fit1)
## the ANOVA table shows that RMRF and SMB are significant in this regression on MSFT stock 

summary(fit2)
anova(fit2)
# the ANOVA table shows that RMRF and HML are significant in this regression on IBM stock 


####### 6 #######
step(fit1)
## find out the best model for MSFT
M_excs <- lm(formula = excs_rtn1 ~ RMRF + SMB, data = new_dat)
summary(M_excs)
#Adjusted R-squared:  0.4748 , RMRF is *** ,SMB ** significant for MSFT
#This means the trend of the stock follows the market trend. Here SMB is less than 0, this shows 
#Microfost's excess earning is affected by large-cap stocks.
#the value of MSFT larger, the more excess earning MSFT will get

step(fit2)
## find out the best model for IBM
I_excs <- lm(formula = excs_rtn2 ~ RMRF + SMB + HML, data = new_dat)
summary(I_excs)
#Adjusted R-squared:  0.3653, RMRF is *** ,HML * significant for MSFT
#Here SMB is not significant for Pvalue is larger  than 0.1
#the value of MSFT larger, the more excess earning MSFT will get


##### 7 #######
### market model
M_mrk <- lm(formula = excs_rtn1 ~ RMRF, data = new_dat)
summary(M_mrk)
# Adjusted R-squared:  0.4284

I_mrk <- lm(formula = excs_rtn2 ~ RMRF, data = new_dat)
summary(I_mrk)
# Adjusted R-squared:  0.329

# we can see 3 factor model outperforms the market model,cause its R square is larger than market model.
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
durbinWatsonTest(M_excs)  #p-value = 0.016  independent in the 99% confidence interval
durbinWatsonTest(I_excs)  #p-value = 0.136  independent in the 90%,95%,99% confidence interval
# test of normality
shapiro.test(M_excs$residuals) #p-value = 0.2232   normal in the 90%,95%,99% confidence interval
shapiro.test(I_excs$residuals) #p-value = 2.739e-05  abnormal in the 90%,95%,99% confidence interval
# test of homoscedasticity 
ncvTest(M_excs)  #p-value = 0.95385   homo in the 90%,95%,99% confidence interval
ncvTest(I_excs)  #p-value = 0.18437   homo in the 90%,95%,99% confidence interval
