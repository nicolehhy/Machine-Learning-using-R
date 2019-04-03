install.packages("MASS")
install.packages("leaps")
install.packages("knitr")
library(MASS)
library(leaps)
library(knitr)

set.seed(123)
dat <- read.csv('C:/STONY/Group_59.csv', header=TRUE, sep=",")
dim(dat)

##find correlation between Y and variables
corr<-NULL
for(i in 1:20){
  corr[i]<-cor(dat$Y,dat[,i+1])
}
corr

##find correlation between variables
cor(dat[,-c(1,2)])


### check the normability 
qqnorm(dat$Y)
shapiro.test(dat$Y) #p-value < 2.2e-16 close to 0, not normal distirbution

### using TA's methond to double chek the normability
M_raw <- lm( Y ~ (E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^3, data=dat )
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')  # most of the ponits gather on the left side

### using boxcox to do the transformation
library(MASS)
boxcox(M_raw)
#lambda close to 0 ,we set lambda to be 0

# do the log transformation
M_trans <- lm( I(log(Y)) ~ (E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^3, data=dat )
shapiro.test(M_trans$residuals)  #p-value = 0.02562
summary(M_trans)$adj.r.square
plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')
summary(M_trans)

## use regsubset to get the most significant variables from the 3power model

library(kableExtra)
library(leaps)
M <- regsubsets( model.matrix(M_trans)[,-1], I(log(dat$Y)), 
                 nbest = 1 , nvmax=5, 
                 method = 'forward', intercept = TRUE )
temp <- summary(M)

Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1, 
                  function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
      caption='Model Summary')

### E3+E5+E2:E3:G2+E2:E3:G7+G3:G8:G13
###  E3,E5,E2,G3,G7,G8,G13
# E1+E3+E5+G3:G8:G13

M_main <- lm( I(log(Y)) ~ E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15, data=dat)
# . here means include all variable from E1 to E5 and from G1 to G15 to the model
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')
## E1,E3,E5,G3,G8,G13, E2 and G7 did't occur

M_2nd <- lm( I(log(Y)) ~ (E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^2, data=dat)
temp  <- summary(M_2nd)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='2nd Interaction')
## E3,E5,G3,G8,G13,E3:G15,G2:G15,G3:G8,G3:G13    E2 and G7 did't occur
## E3,E5,G3,G8,G13,G15

M_2stage <- lm( I(log(Y)) ~ (E1+E2+E3+E5+G2+G3+G7+G8+G13+G15)^3, data=dat)
temp <- summary(M_2stage)
temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]

# out put: E3, G3:G8:G13 

## MODEL: formula = I(log(Y)) ~ E3 + G3:G8:G13




############################### 
##### LASSO ###################
###############################

## we also use lasso to double check our result above
## lasso model 
install.packages("glmnet")
library(glmnet)

## check main effect of the vaiables
f1 <- as.formula( I(log(Y))~ .)
x1 <- model.matrix(f1, dat)
cvfit1 <- cv.glmnet(x1, I(log(dat$Y)))
c1 <- coef(cvfit1, s = "lambda.1se") 
c1
#E1, E2,E3, E5,G3,G8,G13

## check main effect containing two-way interactions
f2 <- as.formula(I(log(Y))~.^2)
x2 <- model.matrix(f2, dat)
cvfit2 <- cv.glmnet(x2, I(log(dat$Y)))
c2 <- coef(cvfit2, s = "lambda.1se") 
c2
#E1*, E2,E3*,E4,E5,G1,G2,G3*,G4,G5,G7,G6,G8*,G10,G11,G13*,G14,G15


## check main effect containing three-way interactions
f3 <- as.formula(I(log(Y))~.^3)
x3 <- model.matrix(f3, dat)
cvfit3 <- cv.glmnet(x3, I(log(dat$Y)))
c3 <- coef(cvfit3, s = "lambda.1se") 
c3
#


## add the significant variables togethor and build the model to double check
fit4 <- lm(Y ~ E3+E5+G3+G8+G13,data=dat)
summary(fit4)
BIC(fit4)



