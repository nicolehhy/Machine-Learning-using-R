##import data
mydat<-read.table("D:/SBU ams/578 regerssion/program/data/IDYGE.csv",header=T,sep=',')
str(mydat)
newdat<-mydat[,-c(1,2)]
attach(newdat)
shapiro.test(Y)
qqnorm(Y)
##p-value=0.8311, y is normal

r<-NULL
for(i in 1:31){
  fit<-lm(Y~newdat[,i+1],data=newdat)
  r[i]<-summary(fit)$adj.r.squared
}

##find correlation between Y and variables
corr<-NULL
for(i in 1:31){
  corr[i]<-cor(Y,newdat[,i+1])
}
corr

#library(car)
#scatterplotMatrix(newdat)

fit2<-lm(Y~(.)^2,data=newdat)
summary(fit2)
fit3<-lm(Y~(E1+E2+E3+E5+E6++G7+G10+G13+G14+G16+G17+G20+G23+G25)^2,data=newdat)
summary(fit3)
step(fit3)
fit4<-lm(Y ~ E1 + E2 + E3 + E5 + E6 + G7 + G10 + G13 + G14 + 
           G16 + G20 + G23 + G25 + E1:G13 + E1:G20 + E1:G25 + E2:G13 + 
           E2:G23 + E2:G25 + E3:G23 + E5:G10 + E5:G23 + E6:G23 + G10:G16 + 
           G13:G14 + G13:G16 + G14:G16 + G20:G23, data = newdat)
summary(fit4)
fit5<-lm(Y~(E1 + E2 + E5 + E6 + G10+ G16 + G20 + G23 + G25)^3,data=newdat )
summary(fit5)
step(fit5)
fit6<-lm(formula = Y ~ E1 + E2 + E5 + E6 + G10 + G16 + G20 + G23 + 
     G25 + E1:E2 + E1:E5 + E1:E6 + E1:G10 + E1:G16 + E1:G20 + 
     E1:G23 + E1:G25 + E2:E5 + E2:E6 + E2:G23 + E2:G25 + E5:E6 + 
     E5:G10 + E5:G16 + E5:G20 + E5:G23 + E5:G25 + E6:G10 + E6:G20 + 
     E6:G23 + E6:G25 + G10:G16 + G10:G20 + G10:G23 + G10:G25 + 
     G16:G20 + G16:G23 + G20:G23 + G20:G25 + G23:G25 + E1:E2:E6 + 
     E1:E2:G23 + E1:E2:G25 + E1:E5:E6 + E1:E5:G16 + E1:E5:G23 + 
     E1:E6:G10 + E1:E6:G20 + E1:E6:G23 + E1:G10:G23 + E2:E5:G25 + 
     E5:E6:G10 + E5:G16:G20 + E6:G20:G25 + G10:G16:G20 + G10:G16:G23 + 
     G10:G20:G23 + G10:G20:G25 + G16:G20:G23 + G20:G23:G25, data = newdat)
summary(fit6)

null <- lm(Y~1,data=newdat)
full <- lm(Y~(E1+E2+E3+E5+E6++G7+G10+G13+G14+G16+G17+G20+G23+G25)^3,data=newdat)
step(null,scope=list(upper=full),data=newdat,direction='backward')
fit<-lm(Y~1,data=newdat)
summary(fit)
par(mfrow=c(1,3))
plot(fit,cex.lab=2)

var(newdat[,2:26])
##residual distribution
var(Y-15.44)
mean(Y-15.44)