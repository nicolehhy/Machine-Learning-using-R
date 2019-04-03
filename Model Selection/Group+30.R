IDE=read.csv("IDEgroup30.csv")
IDG=read.csv("IDGgroup30.csv")
IDY=read.csv("IDYgroup30.csv")
install.packages("car")

Y=IDY$Y
#merge data
tmp=merge(IDE,IDG,by="ID")
dataset=merge(IDY,tmp,by="ID")
dataset=na.omit(dataset)

#check correlation
e=cbind(dataset[,5:10],dataset$Y)
cor(e)
g=cbind(dataset[,12:36],dataset$Y)
cor(g)


#Test if Y is normal
shapiro.test(dataset$Y)
#Y is not normal. Normalize Y with Box-Cox
library(MASS)
min=min(dataset$Y)
dataset$Y=dataset$Y-min(dataset$Y)+1
bc=boxcox(lm(dataset$Y~.,data=data.frame(dataset), plotit=TRUE))
lambda=bc$x[bc$y==max(bc$y)]
#lambda=0.8686869
library(car)
dataset$Y=bcPower(dataset$Y,lambda)
shapiro.test(dataset$Y)
#p.value=0.1986, normal
#lambda=0.868686


#Fit all 31 groups with power 1 and 2
fit1=lm(dataset$Y~.,data=dataset)
fit2=lm(dataset$Y~.^2,data=dataset)

null=lm(dataset$Y~1,data=dataset)

#stepwise function
step(null,scope=list(lower=null, upper=fit1),data=dataset,direction='both')
#lm(formula = dataset$Y ~ E2 + E6 + E3 + E4 + G15 + G17 + G5 + G23 + G10, data = dataset)
summary(step(null,scope=list(lower=null, upper=fit2),data=dataset,direction='both'))
#lm(formula = dataset$Y ~ E2 + E6 + E3 + E4 + G15 + G23 + G2 + G24 + G10 + G25 + E2:E6 + E2:E3 + E6:G15 + G23:G10 + E2:G10 + 
#                         E6:G2 + G24:G25 + G23:G25, data = dataset)

#choose significant terms:
#E2 + E3 + E6 + G23 + G10 + G25 + E2:E6 + E2:E3 + G23:G10
#perform regression with power 3 and 4 to this smaller group:
fit3 = lm(formula = dataset$Y ~ (E2 + E3 + E6 + G23 + G10 + G25 + E2:E6 + E2:E3 + G23:G10)^3,data=dataset)
summary(step(null,scope=list(lower=null, upper=fit3),data=dataset,direction='both'))
fit4 =  lm(formula = dataset$Y ~ (E2 + E3 + E6 + G23 + G10 + G25 + E2:E6 + E2:E3 + G23:G10)^4,data=dataset)
summary(step(null,scope=list(lower=null, upper=fit4),data=dataset,direction='both'))
#R.square=0.3355
#It seems the model does not contain 3 or higher power terms

#Delete terms with one or no star(not significant)
fit.final= lm(formula = dataset$Y ~ G25 + E2:E6 + E2:E3 + G23:G10, data=dataset)
summary(step(null,scope=list(lower=null, upper=fit.final),data=dataset,direction='both'))
#R.square=0.3311
#Delete elements with two stars (not so significant)
fit.final= lm(formula = dataset$Y ~ E2:E6 + E2:E3, data=dataset)
summary(step(null,scope=list(lower=null, upper=fit.final),data=dataset,direction='both'))
#R.square=0.3273
anova(fit.final)
plot(fit.final)








