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
return1 <- c()
for (i in 2:105){
  if(i==1){
    return1[i] <- data[,2][1] - data[,2][1]
  }
  else{
    return1[i] <- (data[,2][i] - data[,2][i-1])/data[,2][i]
  }
}

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


####### 3 #######
###  pairwise scatter plot  ###
plot(I_rtn~MRF,data=new_dat)
plot(I_rtn~SMB,data=new_dat)
plot(I_rtn~HML,data=new_dat)
plot(M_rtn~MRF,data=new_dat)
plot(M_rtn~SMB,data=new_dat)
plot(M_rtn~HML,data=new_dat)
pairs(new_dat[,c(5,6,4)])

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
step(fit2)
## find out the best model for IBM
I_excs <- lm(formula = excs_rtn2 ~ RMRF + SMB + HML, data = new_dat)