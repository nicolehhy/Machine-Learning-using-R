setwd("C:/STONY/Practice/R (No.8)")
dat <- read.table("Group_193.csv", header=TRUE, sep=",")
## normal test, no need to do boxcox
shapiro.test(dat$Y)
qqnorm(dat$Y)

## plot the residual, no need to do boxcox
M_raw <- lm( Y ~ (E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^3, data=dat )
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')


##find correlation between Y and variables
corr<-NULL
for(i in 1:20){
  corr[i]<-cor(dat$Y,dat[,i+1])
}
corr

##find correlation between variables
cor(dat[,-c(1,2)])


## use regsubset to get the most significant variables from the 3power model
install.packages("leaps")
library(leaps)
install.packages("kableExtra")
library(kableExtra)
M <- regsubsets( model.matrix(M_raw)[,-1], dat$Y, 
                 nbest = 1 , nvmax=5, 
                 method = 'forward', intercept = TRUE )
temp <- summary(M)

Var <- colnames(model.matrix(M_raw))
M_select <- apply(temp$which, 1, 
                  function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
      caption='Model Summary')
## E1+E2+E3+E5+G3+G4+G5+G10 SIGNIFICANT(R^2 larger, and BIC smaller)
## E1+E2+E5 MOST SIGNIFICANT(there is not very large difference among 3rd,4th and 5th model,so 3rd is best)


## in order to know if the result of the 3 power model is the best,we need to do 1 and 2 power to double check
# . here means include all variable from E1 to E5 and from G1 to G15 to the model
M_main <- lm( Y ~ ., data=dat)
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')
## E1+E2+E5 SIGNIFICANT

M_2nd <- lm( Y ~ (.)^2, data=dat)
temp  <- summary(M_2nd)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='2nd Interaction')
## E2+E5

## do 3 power to double check
M_2stage <- lm( Y ~ (E1+E2+E3+E5+G3+G4+G5+G10)^3, data=dat)
temp <- summary(M_2stage)
temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]


### output model: Y=??0+??1E2+??2E5




############################### 
##### LASSO ###################
###############################

## we also use lasso to double check our result above
## lasso model 
install.packages("glmnet")
library(glmnet)

## check main effect of the vaiables
f1 <- as.formula(Y~ .)
x1 <- model.matrix(f1, dat)
cvfit1 <- cv.glmnet(x1, dat$Y)
c1 <- coef(cvfit1, s = "lambda.1se") 
c1
#E1, E2, E5

## check main effect containing two-way interactions
f2 <- as.formula(Y~ .^2)
x2 <- model.matrix(f2, dat)
cvfit2 <- cv.glmnet(x2, dat$Y)
c2 <- coef(cvfit2, s = "lambda.1se") 
c2
#E1, E2, E5


## check main effect containing three-way interactions
f3 <- as.formula(Y~ .^3)
x3 <- model.matrix(f3, dat)
cvfit3 <- cv.glmnet(x3, dat$Y)
c3 <- coef(cvfit3, s = "lambda.1se") 
c3
#E5


## add the significant variables togethor and build the model to double check
fit4 <- lm(Y ~ E1 + E2 + E5,data=dat)
summary(fit4)
#E1, E2, E5 signiificant  
# Adjusted R-squared = 0.57
BIC(fit4) # -6254.255 < -1657.46307952179(which appears in the first kable())

### output model: Y=??0+??1E1+??2E2+??3E5
