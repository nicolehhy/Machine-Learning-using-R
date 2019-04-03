library(dplyr)
library(scales) 
library(ggplot2)
fund <- read.csv("C:/STONY/Practice/R (No.9)/fund.csv", header = T)
str(fund)
### checking 
f <- fund %>%
  filter(max(as.Date(End_Date,format='%m/%d/%Y'))== max(as.Date(Date,format='%m/%d/%Y')))
### there is no project which was stopped before the end date. 

### clean the data ###
### subset the data 
fund1 <- fund %>%
  filter(as.Date(End_Date,format='%m/%d/%Y')==as.Date(Date,format='%m/%d/%Y'),Goal >= 5000,Backers >=1, Duration >= 10)
### 617 usable projects


### calculate the table 2
fund2 <- fund1 %>%
  group_by(Title,Category) %>%
  summarise (goal = Goal,amount = Contributions,backers = Backers,
             avg_contri = Contributions/Backers,
             per = Contributions/Goal*100,num_fri = Social_Network_Size,day = Duration) 

# Campaign goal
summary(fund2$goal)
sd(fund2$goal)
# Amount raised
summary(fund2$amount)
sd(fund2$amount)
# Number of backers
summary(fund2$backers)
sd(fund2$backers)
# Average contribution per backer
summary(fund2$avg_contri)
sd(fund2$avg_contri)
# Percent of campaign goal raised
summary(fund2$per)
sd(fund2$per)
# Number of facebook friends
summary(fund2$num_fri)
sd(fund2$num_fri)
# Number of days
summary(fund2$day)
sd(fund2$day)


### the distribution of campaigns across categories Fig2###
category <- fund1 %>%
  group_by(Category) %>%
  summarise (n = n())
str(category)

#@@@@ change the " to others
ggplot(category,aes(Category,n,fill=Category)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Category") +
  scale_y_continuous(breaks=seq(0, 160, 20))

### Normalized contribution dynamics of crowfunding campaigns Fig4 ###

fund3 <- subset(fund, ID = fund1$ID)

### calculate duartion % and progress %

fund3 <- fund3 %>% 
  group_by(ID) %>% 
  mutate(dura = as.numeric((as.Date(Date,format='%m/%d/%Y')- as.Date(Start_Date,format='%m/%d/%Y'))/max(Duration)*100))
max(fund3$dura)

fund3 <- fund3 %>% group_by(ID) %>% mutate(prog = Contributions/Goal*100)
fund33 <- fund3
fund33 <- fund33 %>% filter(Funding_Status <= 1)

### In order to plot, we have to subset the data into success and unsuccess
fund330 <- fund33 %>% filter(Funding_Status <= 0)
fund331 <- fund33 %>% filter(Funding_Status == 1)
### calculate the mean trajectory of success and unsuccess
fund_mean1 <- fund3 %>% filter(Funding_Status == 1) %>% group_by(dura) %>%
  summarise(m_prog=mean(prog))
fund_mean0 <- fund3 %>% filter(Funding_Status == 0) %>% group_by(dura) %>%
  summarise(m_prog=mean(prog))

library(gridExtra)

### @@@@@ smooth !!!!!!!!!
fund33_sub <- fund33 %>% filter(prog<=150)
max(fund33_sub$prog)
p1 <- ggplot() +
  geom_smooth(aes(x=dura, y=prog, group=ID, color=as.character(Funding_Status)),span=1,se=FALSE,fund33_sub) +
  geom_smooth(aes(x=dura, y=m_prog, method="gam",colour = "red", lwd=1),se=FALSE,fund_mean1) +
  geom_smooth(aes(x=dura, y=m_prog, method="gam",colour = "red", lwd=1),fund_mean0) +
  ylim(c(0,150)) +
  theme(legend.position="none") 

fund331_sub <- fund331 %>% filter(prog<=150)
max(fund331_sub$prog)
p2 <- ggplot() +
  geom_smooth(aes(x=dura, y=prog, group=ID),span=1,se=FALSE,fund331_sub) +
  geom_smooth(aes(x=dura, y=m_prog, method="gam",colour = "red", lwd=1),fund_mean1) +
  ylim(c(0,150)) +
  theme(legend.position="none")

p3 <- ggplot() +
  geom_smooth(aes(x=dura, y=prog, group=ID),se=FALSE,fund330) +
  geom_smooth(aes(x=dura, y=m_prog, method="gam",colour = "red", lwd=1),fund_mean0) +
  ylim(c(0,150)) +
  theme(legend.position="none")

grid.arrange(p1, p2, p3, nrow = 1)



#### for successful projects# cross-sectional
library(fdapace)
fund331 <- fund331[-2412,]
Files1 <- MakeFPCAInputs(fund331$ID, fund331$dura, fund331$prog)
fpcaObjFlies1 <- FPCA(Files1$Ly, Files1$Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=25, FVEthreshold=1))
fittedCurvesP11 <- fitted(fpcaObjFlies1, K=4, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP12 <- fitted(fpcaObjFlies1, K=4, derOptns=list(p=2, kernelType='gauss'))

#### for unsuccessful projects 
Files0 <- MakeFPCAInputs(fund330$ID, fund330$dura, fund330$prog)
fpcaObjFlies0 <- FPCA(Files0$Ly, Files0$Lt, list(plot=TRUE, methodMuCovEst='smooth'))
fittedCurvesP01 <- fitted(fpcaObjFlies0, derOptns=list(p=1, kernelType='epan'))
fittedCurvesP02 <- fitted(fpcaObjFlies0, derOptns=list(p=2, kernelType='epan'))

### plot the velocity for success and unsuccess
### get the velocity dataframe for success
t_temp1 <- rep(fpcaObjFlies1$workGrid, length(Files1$Lid))
temp1 <- matrix(t(fittedCurvesP11), ncol=1)
id1 <- rep(1:length(Files1$Lid), each=25)
d1_mat1 <- data.frame(cbind(id1, t_temp1, temp1))
### get the velocity dataframe for unsuccess
t_temp0 <- rep(fpcaObjFlies0$workGrid, length(Files0$Lid))
temp0 <- matrix(t(fittedCurvesP01), ncol=1)
id0 <- rep(1:length(Files0$Lid), each=25)
d1_mat0 <- data.frame(cbind(id0, t_temp0, temp0))

### plot the velocity 
der11 <- ggplot(d1_mat1, aes(x=t_temp1, y=temp1, group=id1)) +
  geom_line(aes(colour="red")) +
  theme(legend.position="none")+ 
  ylab(label="Contribution Velocity") + 
  xlab("Duration %")

der01 <- ggplot(d1_mat0, aes(x=t_temp0, y=temp0, group=id0)) +
  geom_line(aes(colour="red")) +
  theme(legend.position="none")+ 
  ylab(label="Contribution Velocity") + 
  xlab("Duration %")
grid.arrange(der11, der01, nrow = 1)

### plot the velocity boxplot of success and unsuccess
dim(temp1)
dim(temp0)
temp1 <- cbind(temp1,rep(1,times=7300))
temp0 <- cbind(temp0,rep(0,times=16626))
velocity <- as.data.frame(rbind(temp1,temp0))
colnames(velocity) <- c("velocity","category")

ggplot(velocity, aes(category, velocity, fill=factor(category))) +
  geom_boxplot() +
  ylim(c(0,3))

#### plot the accelaration of success and unsuccess

t_temp1 <- rep(fpcaObjFlies1$workGrid, length(Files1$Lid))
temp1 <- matrix(t(fittedCurvesP12), ncol=1)
id1 <- rep(1:length(Files1$Lid), each=25)
d1_mat1 <- data.frame(cbind(id1, t_temp1, temp1))

t_temp0 <- rep(fpcaObjFlies0$workGrid, length(Files0$Lid))
temp0 <- matrix(t(fittedCurvesP02), ncol=1)  
id0 <- rep(1:length(Files0$Lid), each=25)
d1_mat0 <- data.frame(cbind(id0, t_temp0, temp0))

der12 <- ggplot(d1_mat1, aes(x=t_temp1, y=temp1, group=id1)) +
  geom_line(aes(colour="red")) +
  theme(legend.position="none") + 
  ylab(label="Contribution Velocity") + 
  xlab("Duration %")


der02 <- ggplot(d1_mat0, aes(x=t_temp0, y=temp0, group=id0)) +
  geom_line(aes(colour="red")) +
  theme(legend.position="none") +
  ylab(label="Contribution Accelleration") + 
  xlab("Duration %")

library(gridExtra)
grid.arrange(der12, der02, nrow = 1)

### plot the accelleration boxplot of success and unsuccess
dim(temp1)
dim(temp0)
temp1 <- cbind(temp1,rep(1,times=7300))
temp0 <- cbind(temp0,rep(0,times=16626))
accelleration <- as.data.frame(rbind(temp1,temp0))
colnames(accelleration) <- c("accelleration","category")

ggplot(accelleration, aes(category, accelleration, fill=factor(category))) +
  geom_boxplot() +
  ylim(c(-0.02,0.01))



### build prediction model 

### get the new data set which contains velocity,accelaration, contractory, gaols.


fund_331 <- fund331[-2412,]
fund_model <- as.data.frame(rbind(fund_331,fund330))


######################
####  LINEAR model ###
######################
### set mape function first, cause we are gonna use it later
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))
  return (mape)
}
fund_model10 <- fund_model %>% filter(dura <= 10)
fund_model30 <- fund_model %>% filter(dura <= 30)
fund_model50 <- fund_model %>% filter(dura <= 50)
fund_model70 <- fund_model %>% filter(dura <= 70)
fund_model90 <- fund_model %>% filter(dura <= 90)
### 10 percent of model ###
LR_dat10 <- fund_model10 %>% group_by(ID) %>% summarise(bak = max(Backers),
                                                        contri = max(Contributions),
                                                        soci = mean(Social_Network_Size),
                                                        goal = mean(Goal))
outcome <- fund_model %>% group_by(ID) %>% summarise(out = max(Contributions))
LR_dat10 <- cbind(y = outcome$out,
                  bak = LR_dat10[,2],
                  contri = LR_dat10[,3],
                  soci = LR_dat10[,4],
                  goal = LR_dat10[,5])
LR10 <- lm(y ~ bak+contri+goal+soci, data=as.data.frame(LR_dat10[c(1:518),]))
summary(LR10) 
AIC(LR10)
BIC(LR10)
# calculate the mape of LR10
### predict 10 percent of campaign with 519-618 obs ###
new <- as.data.frame(LR_dat10[c(519:618),])
pred <- predict(LR10, new, se.fit = TRUE)
actual <-  as.data.frame(LR_dat10[c(519:618),])$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
LR10.mape <- mape(data_mape$y,data_mape$yhat)


### 30 percent of model ###
LR_dat30 <- fund_model30 %>% group_by(ID) %>% summarise(bak = max(Backers),
                                                        contri = max(Contributions),
                                                        soci = mean(Social_Network_Size),
                                                        goal = mean(Goal))
outcome <- fund_model %>% group_by(ID) %>% summarise(out = max(Contributions))
LR_dat30 <- cbind(out = outcome[,2],
                  bak = LR_dat30[,2],
                  contri = LR_dat30[,3],
                  soci = LR_dat30[,4],
                  goal = LR_dat30[,5])
LR30 <- lm(out ~ bak+contri+goal+soci, data=as.data.frame(LR_dat30[c(1:518),]))
summary(LR30)
AIC(LR30)
BIC(LR30)

# calculate the mape of LR30
### predict 30 percent of campaign with 519-618 obs ###
new <- as.data.frame(LR_dat30[c(519:618),])
pred <- predict(LR30, new, se.fit = TRUE)
actual <-  as.data.frame(LR_dat30[c(519:618),])$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
LR30.mape <- mape(data_mape$y,data_mape$yhat)


### 50 percent of model ###
LR_dat50 <- fund_model50 %>% group_by(ID) %>% summarise(bak = max(Backers),
                                                        contri = max(Contributions),
                                                        soci = mean(Social_Network_Size),
                                                        goal = mean(Goal))
outcome <- fund_model %>% group_by(ID) %>% summarise(out = max(Contributions))
LR_dat50 <- cbind(out = outcome[,2],
                  bak = LR_dat50[,2],
                  contri = LR_dat50[,3],
                  soci = LR_dat50[,4],
                  goal = LR_dat50[,5])
LR50 <- lm(out ~ bak+contri+goal+soci, data=as.data.frame(LR_dat50[c(1:518),]))
summary(LR50)
AIC(LR50)
BIC(LR50)

# calculate the mape of LR50
### predict 50 percent of campaign with 519-618 obs ###
new <- as.data.frame(LR_dat30[c(519:618),])
pred <- predict(LR50, new, se.fit = TRUE)
actual <-  as.data.frame(LR_dat50[c(519:618),])$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
LR50.mape <- mape(data_mape$y,data_mape$yhat)


### 70 percent of model ###
LR_dat70 <- fund_model70 %>% group_by(ID) %>% summarise(bak = max(Backers),
                                                        contri = max(Contributions),
                                                        soci = mean(Social_Network_Size),
                                                        goal = mean(Goal))
outcome <- fund_model %>% group_by(ID) %>% summarise(out = max(Contributions))
LR_dat70 <- cbind(out = outcome[,2],
                  bak = LR_dat70[,2],
                  contri = LR_dat70[,3],
                  soci = LR_dat70[,4],
                  goal = LR_dat70[,5])
LR70 <- lm(out ~ bak+contri+goal+soci, data=as.data.frame(LR_dat70[c(1:518),]))
summary(LR70)
AIC(LR70)
BIC(LR70)

# calculate the mape of LR70
### predict 70 percent of campaign with 519-618 obs ###
new <- as.data.frame(LR_dat70[c(519:618),])
pred <- predict(LR70, new, se.fit = TRUE)
actual <-  as.data.frame(LR_dat70[c(519:618),])$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
LR70.mape <- mape(data_mape$y,data_mape$yhat)


### 90 percent of model ###
LR_dat90 <- fund_model90 %>% group_by(ID) %>% summarise(bak = max(Backers),
                                                        contri = max(Contributions),
                                                        soci = mean(Social_Network_Size),
                                                        goal = mean(Goal))
outcome <- fund_model %>% group_by(ID) %>% summarise(out = max(Contributions))
LR_dat90 <- cbind(out = outcome[,2],
                  bak = LR_dat90[,2],
                  contri = LR_dat90[,3],
                  soci = LR_dat90[,4],
                  goal = LR_dat90[,5])
LR90 <- lm(out ~ bak+contri+goal+soci, data=as.data.frame(LR_dat90[c(1:518),]))
summary(LR90)
AIC(LR90)
BIC(LR90)

# calculate the mape of LR90
### predict 90 percent of campaign with 519-618 obs ###
new <- as.data.frame(LR_dat90[c(519:618),])
pred <- predict(LR90, new, se.fit = TRUE)
actual <-  as.data.frame(LR_dat90[c(519:618),])$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
LR90.mape <- mape(data_mape$y,data_mape$yhat)



#######################
#### FDA LINEAR Model #
#######################

## 10 percent of campaign 
Files_model <- MakeFPCAInputs(fund_model$ID, fund_model$dura, fund_model$prog)
library(fdapace)
fund_model10 <- fund_model %>% filter(dura <= 10)
Files_model10 <- MakeFPCAInputs(fund_model10$ID, fund_model10$dura, fund_model10$prog)
duration_span <- 5
outcome10 <- sapply(Files_model$Ly, function(x) x[length(x)])
goal <- fund_model10 %>%  group_by(ID) %>% summarise(goal=mean(Goal))
goal <- data.frame(goal[,2])
fpcaObjFlies_model10 <- FPCA(Files_model10 $Ly, Files_model10 $Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=10, FVEthreshold=1))
Contri_pc_score <- fpcaObjFlies_model10$xiEst
fittedCurvesP_m010 <- fitted(fpcaObjFlies_model10, K=2, derOptns=list(p=0, kernelType="gauss"))
fittedCurvesP_m110 <- fitted(fpcaObjFlies_model10, K=2, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP_m210 <- fitted(fpcaObjFlies_model10, K=2, derOptns=list(p=2, kernelType='gauss'))

DM10 <- cbind( y      = outcome10,
             contri = fittedCurvesP_m010[,1], 
             vel    = fittedCurvesP_m110[,1], 
             acce   = fittedCurvesP_m210[,1], 
             goal   = goal) 

FDA10 <- lm(y ~ contri+vel+goal, data=as.data.frame(DM10[c(1:518),]))
summary(FDA10)
AIC(FDA10)
BIC(FDA10)
# Adjusted R-squared: 0.9979 

# calculate the mape of FDA10
### predict 10 percent of campaign with 519-618 obs ###
new <- DM10[c(519:618),]
pred <- predict(FDA10, new, se.fit = TRUE)
actual <- LR_dat10[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDA10.mape <- mape(data_mape$y,data_mape$yhat)


## 30 percent of campaign 
fund_model30 <- fund_model %>% filter(dura <= 30)
Files_model30 <- MakeFPCAInputs(fund_model30$ID, fund_model30$dura, fund_model30$prog)
duration_span <- 5
outcome30 <- sapply(Files_model$Ly, function(x) x[length(x)])
goal <- fund_model30 %>%  group_by(ID) %>% summarise(goal=mean(Goal))
goal <- data.frame(goal[,2])
fpcaObjFlies_model30 <- FPCA(Files_model30 $Ly, Files_model30 $Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=10, FVEthreshold=1))
Contri_pc_score30 <- fpcaObjFlies_model30$xiEst
fittedCurvesP_m030 <- fitted(fpcaObjFlies_model30, K=2, derOptns=list(p=0, kernelType="gauss"))
fittedCurvesP_m130 <- fitted(fpcaObjFlies_model30, K=2, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP_m230 <- fitted(fpcaObjFlies_model30, K=2, derOptns=list(p=2, kernelType='gauss'))

DM30 <- cbind( y      = outcome30,
               contri = fittedCurvesP_m030[,3], 
               vel    = fittedCurvesP_m130[,3], 
               acce   = fittedCurvesP_m230[,3], 
               goal   = goal) 

FDA30 <- lm(y ~ contri+vel+goal, data=as.data.frame(DM30[c(1:518),]))
summary(FDA30)
AIC(FDA30)
BIC(FDA30)
# Adjusted R-squared:  0.9997 
# calculate the mape of FDA30
### predict 30 percent of campaign with 519-618 obs ###
new <- DM30[c(519:618),]
pred <- predict(FDA30, new, se.fit = TRUE)
actual <- LR_dat30[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDA30.mape <- mape(data_mape$y,data_mape$yhat)


## 50 percent of campaign 
fund_model50 <- fund_model %>% filter(dura <= 50)
Files_model50 <- MakeFPCAInputs(fund_model50$ID, fund_model50$dura, fund_model50$prog)
duration_span <- 5
outcome50 <- sapply(Files_model$Ly, function(x) x[length(x)])
goal <- fund_model50 %>%  group_by(ID) %>% summarise(goal=mean(Goal))
goal <- data.frame(goal[,2])
fpcaObjFlies_model50 <- FPCA(Files_model50 $Ly, Files_model50 $Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=10, FVEthreshold=1))
Contri_pc_score50 <- fpcaObjFlies_model50$xiEst
fittedCurvesP_m050 <- fitted(fpcaObjFlies_model50, K=2, derOptns=list(p=0, kernelType="gauss"))
fittedCurvesP_m150 <- fitted(fpcaObjFlies_model50, K=2, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP_m250 <- fitted(fpcaObjFlies_model50, K=2, derOptns=list(p=2, kernelType='gauss'))

DM50 <- cbind( y      = outcome50,
               contri = fittedCurvesP_m050[,5], 
               vel    = fittedCurvesP_m150[,5], 
               acce   = fittedCurvesP_m250[,5], 
               goal   = goal) 

FDA50 <- lm(y ~ contri+vel+goal, data=as.data.frame(DM50[c(1:518),]) )
summary(FDA50)
AIC(FDA50)
BIC(FDA50)
# Adjusted R-squared:  0.9987
# calculate the mape of FDA50
### predict 50 percent of campaign with 519-618 obs ###
new <- DM50[c(519:618),]
pred <- predict(FDA50, new, se.fit = TRUE)
actual <- LR_dat50[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDA50.mape <- mape(data_mape$y,data_mape$yhat)


## 70 percent of campaign 
fund_model70 <- fund_model %>% filter(dura <= 70)
Files_model70 <- MakeFPCAInputs(fund_model70$ID, fund_model70$dura, fund_model70$prog)
duration_span <- 5
outcome70 <- sapply(Files_model$Ly, function(x) x[length(x)])
goal <- fund_model70 %>%  group_by(ID) %>% summarise(goal=mean(Goal))
goal <- data.frame(goal[,2])
fpcaObjFlies_model70 <- FPCA(Files_model70 $Ly, Files_model70 $Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=10, FVEthreshold=1))
Contri_pc_score70 <- fpcaObjFlies_model70$xiEst
fittedCurvesP_m070 <- fitted(fpcaObjFlies_model70, K=2, derOptns=list(p=0, kernelType="gauss"))
fittedCurvesP_m170 <- fitted(fpcaObjFlies_model70, K=2, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP_m270<- fitted(fpcaObjFlies_model70, K=2, derOptns=list(p=2, kernelType='gauss'))

DM70 <- cbind( y      = outcome70,
               contri = fittedCurvesP_m070[,7], 
               vel    = fittedCurvesP_m170[,7], 
               acce   = fittedCurvesP_m270[,7], 
               goal   = goal) 

FDA70 <- lm(y ~ contri+vel+goal, data=as.data.frame(DM70[c(1:518),]) )
summary(FDA70)
AIC(FDA70)
BIC(FDA70)
#Adjusted R-squared:  0.9985
# calculate the mape of FDA70
### predict 70 percent of campaign with 519-618 obs ###
new <- DM70[c(519:618),]
pred <- predict(FDA70, new, se.fit = TRUE)
actual <- LR_dat70[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDA70.mape <- mape(data_mape$y,data_mape$yhat)



## 90 percent of campaign 
fund_model90 <- fund_model %>% filter(dura <= 90)
Files_model90 <- MakeFPCAInputs(fund_model90$ID, fund_model90$dura, fund_model90$prog)
duration_span <- 5
outcome90 <- sapply(Files_model$Ly, function(x) x[length(x)])
goal <- fund_model90 %>%  group_by(ID) %>% summarise(goal=mean(Goal))
goal <- data.frame(goal[,2])
fpcaObjFlies_model90 <- FPCA(Files_model90 $Ly, Files_model90 $Lt, list(plot=TRUE, methodMuCovEst='smooth', nRegGrid=10, FVEthreshold=1))
Contri_pc_score90 <- fpcaObjFlies_model90$xiEst
fittedCurvesP_m090 <- fitted(fpcaObjFlies_model90, K=2, derOptns=list(p=0, kernelType="gauss"))
fittedCurvesP_m190 <- fitted(fpcaObjFlies_model90, K=2, derOptns=list(p=1, kernelType="gauss"))
fittedCurvesP_m290<- fitted(fpcaObjFlies_model90, K=2, derOptns=list(p=2, kernelType='gauss'))

DM90 <- cbind( y      = outcome90,
               contri = fittedCurvesP_m090[,9], 
               vel    = fittedCurvesP_m190[,9], 
               acce   = fittedCurvesP_m290[,9], 
               goal   = goal) 

FDA90 <- lm(y ~ contri+vel+goal, data=as.data.frame(DM90[c(1:518),]) )
summary(FDA90)
AIC(FDA90)
BIC(FDA90)
#Adjusted R-squared:  0.9981
# calculate the mape of FDA90
### predict 90 percent of campaign with 519-618 obs ###
new <- DM90[c(519:618),]
pred <- predict(FDA90, new, se.fit = TRUE)
actual <- LR_dat90[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDA90.mape <- mape(data_mape$y,data_mape$yhat)


#######################
####  FDA+COV model ###
#######################
###  the model with covariance
### 10 PERCENT MODEL ####
soci <- fund_model10 %>%  group_by(ID) %>% summarise(soci=mean(Social_Network_Size))
soci <- data.frame(soci[,2])

DM10 <- cbind( y      = outcome10,
             contri = fittedCurvesP_m010[,1], 
             vel    = fittedCurvesP_m110[,1], 
             acce   = fittedCurvesP_m210[,1], 
             goal   = goal,
             soci   = soci) 
FDACOV10 <- lm(y ~ contri+vel+goal+soci+acce, data=as.data.frame(DM10[c(1:518),]))
summary(FDACOV10)
BIC(FDACOV10)
AIC(FDACOV10)
# Adjusted R-squared:  0.9979 
# calculate the mape of FDACOV10
### predict 10 percent of campaign with 519-618 obs ###
new <- DM10[c(519:618),]
pred <- predict(FDACOV10, new, se.fit = TRUE)
actual <- LR_dat10[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDACOV10.mape <- mape(data_mape$y,data_mape$yhat)


### 30 PERCENT MODEL ####
soci <- fund_model30 %>%  group_by(ID) %>% summarise(soci=mean(Social_Network_Size))
soci <- data.frame(soci[,2])

DM30 <- cbind( y      = outcome30,
               contri = fittedCurvesP_m030[,1], 
               vel    = fittedCurvesP_m130[,1], 
               acce   = fittedCurvesP_m230[,1], 
               goal   = goal,
               soci   = soci) 
FDACOV30 <- lm(y ~ contri+vel+goal+soci+acce, data=as.data.frame(DM30[c(1:518),]))
summary(FDACOV30)
BIC(FDACOV30)
AIC(FDACOV30)
# Adjusted R-squared:  0.9869 
# calculate the mape of FDACOV30
### predict 30 percent of campaign with 519-618 obs ###
new <- DM30[c(519:618),]
pred <- predict(FDACOV30, new, se.fit = TRUE)
actual <- LR_dat30[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDACOV30.mape <- mape(data_mape$y,data_mape$yhat)


### 50 PERCENT MODEL ####
soci <- fund_model50 %>%  group_by(ID) %>% summarise(soci=mean(Social_Network_Size))
soci <- data.frame(soci[,2])

DM50 <- cbind( y      = outcome50,
               contri = fittedCurvesP_m050[,1], 
               vel    = fittedCurvesP_m150[,1], 
               acce   = fittedCurvesP_m250[,1], 
               goal   = goal,
               soci   = soci) 
FDACOV50 <- lm(y ~ contri+vel+goal+soci+acce, data=as.data.frame(DM50[c(1:518),]))
summary(FDACOV50)
BIC(FDACOV50)
AIC(FDACOV50)
# Adjusted R-squared:  0.9987 
# calculate the mape of FDACOV50
### predict 50 percent of campaign with 519-618 obs ###
new <- DM50[c(519:618),]
pred <- predict(FDACOV50, new, se.fit = TRUE)
actual <- LR_dat50[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDACOV50.mape <- mape(data_mape$y,data_mape$yhat)


### 70 PERCENT MODEL ####
soci <- fund_model70 %>%  group_by(ID) %>% summarise(soci=mean(Social_Network_Size))
soci <- data.frame(soci[,2])

DM70 <- cbind( y      = outcome70,
               contri = fittedCurvesP_m070[,1], 
               vel    = fittedCurvesP_m170[,1], 
               acce   = fittedCurvesP_m270[,1], 
               goal   = goal,
               soci   = soci) 
FDACOV70 <- lm(y ~ contri+vel+goal+soci+acce, data=as.data.frame(DM70[c(1:518),]))
summary(FDACOV70)
BIC(FDACOV70)
AIC(FDACOV70)
# Adjusted R-squared:  0.999
# calculate the mape of FDACOV50
### predict 50 percent of campaign with 519-618 obs ###
new <- DM70[c(519:618),]
pred <- predict(FDACOV70, new, se.fit = TRUE)
actual <- LR_dat70[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDACOV70.mape <- mape(data_mape$y,data_mape$yhat)



### 90 PERCENT MODEL ####
soci <- fund_model90 %>%  group_by(ID) %>% summarise(soci=mean(Social_Network_Size))
soci <- data.frame(soci[,2])

DM90 <- cbind( y      = outcome90,
               contri = fittedCurvesP_m090[,1], 
               vel    = fittedCurvesP_m190[,1], 
               acce   = fittedCurvesP_m290[,1], 
               goal   = goal,
               soci   = soci) 
FDACOV90 <- lm(y ~ contri+vel+goal+soci+acce, data=as.data.frame(DM90[c(1:518),]))
summary(FDACOV90)
BIC(FDACOV90)
AIC(FDACOV90)
# Adjusted R-squared:  0.9981 
# calculate the mape of FDACOV50
### predict 50 percent of campaign with 519-618 obs ###
new <- DM90[c(519:618),]
pred <- predict(FDACOV90, new, se.fit = TRUE)
actual <- LR_dat90[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
FDACOV90.mape <- mape(data_mape$y,data_mape$yhat)


###################
####  GAM model ###
###################
# Penalized smooth spline
install.packages('pspline')
library(pspline)

##### 10 percent of progress for the GAM model
f_NumCont_tx_10 <- rep(0, length(unique(fund_model$ID)))
f_AmtCont_tx_10 <- rep(0, length(unique(fund_model$ID)))
marker <- 1
for(i in unique(fund_model$ID)){
  
  dat_temp <- fund_model[fund_model$ID==i,]
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Backers, norder=2)
  f_NumCont_tx_10[marker] <- predict(spline_temp, 10) 
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Contributions, norder=2)
  f_AmtCont_tx_10[marker]  <- predict(spline_temp, 10)
  
  marker <- marker + 1
}

f_10 <- fund_model10 %>% group_by(ID) %>% 
          summarise(soci = max(Social_Network_Size),goal=max(Goal))

DM10 <- cbind( y      = outcome10,
               numcount = f_NumCont_tx_10, 
               amtcount   = f_AmtCont_tx_10, 
               goal   = f_10[,3],
               soci   = f_10[,2]) 
GAM10 <- lm(y ~ numcount+amtcount+goal+soci, data=as.data.frame(DM10[c(1:518),]))
summary(GAM10)
# Adjusted R-squared:  0.9666
AIC(GAM10)
BIC(GAM10)
# calculate the mape of GAM10
### predict 10 percent of campaign with 519-618 obs ###
new <- DM10[c(519:618),]
pred <- predict(GAM10, new, se.fit = TRUE)
actual <- LR_dat10[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
GAM10.mape <- mape(data_mape$y,data_mape$yhat)

##### 30 percent of progress for the GAM model
f_NumCont_tx_30 <- rep(0, length(unique(fund_model$ID)))
f_AmtCont_tx_30 <- rep(0, length(unique(fund_model$ID)))
marker <- 1
for(i in unique(fund_model$ID)){
  
  dat_temp <- fund_model[fund_model$ID==i,]
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Backers, norder=2)
  f_NumCont_tx_30[marker] <- predict(spline_temp, 30) 
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Contributions, norder=2)
  f_AmtCont_tx_30[marker]  <- predict(spline_temp, 30)
  
  marker <- marker + 1
}

f_30 <- fund_model30 %>% group_by(ID) %>% 
  summarise(soci = max(Social_Network_Size),goal=max(Goal))

DM30 <- cbind( y      = outcome30,
                numcount = f_NumCont_tx_30, 
                amtcount   = f_AmtCont_tx_30, 
                goal   = f_30[,3],
                soci   = f_30[,2]) 
GAM30 <- lm(y ~ numcount+amtcount+goal+soci, data=as.data.frame(DM30[c(1:518),]))
summary(GAM30)

AIC(GAM30)
BIC(GAM30)
# calculate the mape of GAM30
### predict 30 percent of campaign with 519-618 obs ###
new <- DM30[c(519:618),]
pred <- predict(GAM30, new, se.fit = TRUE)
actual <- LR_dat30[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
GAM30.mape <- mape(data_mape$y,data_mape$yhat)



##### 50 percent of progress for the GAM model
f_NumCont_tx_50 <- rep(0, length(unique(fund_model$ID)))
f_AmtCont_tx_50 <- rep(0, length(unique(fund_model$ID)))
marker <- 1
for(i in unique(fund_model$ID)){
  
  dat_temp <- fund_model[fund_model$ID==i,]
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Backers, norder=2)
  f_NumCont_tx_50[marker] <- predict(spline_temp, 50) 
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Contributions, norder=2)
  f_AmtCont_tx_50[marker]  <- predict(spline_temp, 50)
  
  marker <- marker + 1
}

f_50 <- fund_model50 %>% group_by(ID) %>% 
  summarise(soci = max(Social_Network_Size),goal=max(Goal))

DM50 <- cbind( y      = outcome50,
                numcount = f_NumCont_tx_50, 
                amtcount   = f_AmtCont_tx_50, 
                goal   = f_50[,3],
                soci   = f_50[,2]) 
GAM50 <- lm(y ~ numcount+amtcount+goal+soci, data=as.data.frame(DM50[c(1:518),]))
summary(GAM50)

AIC(GAM50)
BIC(GAM50)
# calculate the mape of GAM50
### predict 50 percent of campaign with 519-618 obs ###
new <- DM50[c(519:618),]
pred <- predict(GAM50, new, se.fit = TRUE)
actual <- LR_dat50[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
GAM50.mape <- mape(data_mape$y,data_mape$yhat)

##### 70 percent of progress for the GAM model
f_NumCont_tx_70 <- rep(0, length(unique(fund_model$ID)))
f_AmtCont_tx_70 <- rep(0, length(unique(fund_model$ID)))
marker <- 1
for(i in unique(fund_model$ID)){
  
  dat_temp <- fund_model[fund_model$ID==i,]
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Backers, norder=2)
  f_NumCont_tx_70[marker] <- predict(spline_temp, 70) 
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Contributions, norder=2)
  f_AmtCont_tx_70[marker]  <- predict(spline_temp, 70)
  
  marker <- marker + 1
}

f_70 <- fund_model70 %>% group_by(ID) %>% 
  summarise(soci = max(Social_Network_Size),goal=max(Goal))

DM70 <- cbind( y      = outcome70,
                numcount = f_NumCont_tx_70, 
                amtcount   = f_AmtCont_tx_70, 
                goal   = f_70[,3],
                soci   = f_70[,2]) 
GAM70 <- lm(y ~ numcount+amtcount+goal+soci, data=as.data.frame(DM70[c(1:518),]))
summary(GAM70)

# Adjusted R-squared:  0.9552 
AIC(GAM70)
BIC(GAM70)
# calculate the mape of GAM70
### predict 70 percent of campaign with 519-618 obs ###
new <- DM70[c(519:618),]
pred <- predict(GAM70, new, se.fit = TRUE)
actual <- LR_dat70[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
GAM70.mape <- mape(data_mape$y,data_mape$yhat)


##### 90 percent of progress for the GAM model
f_NumCont_tx_90 <- rep(0, length(unique(fund_model$ID)))
f_AmtCont_tx_90 <- rep(0, length(unique(fund_model$ID)))
marker <- 1
for(i in unique(fund_model$ID)){
  
  dat_temp <- fund_model[fund_model$ID==i,]
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Backers, norder=2)
  f_NumCont_tx_90[marker] <- predict(spline_temp, 90) 
  spline_temp  <- smooth.Pspline(dat_temp$dura, dat_temp$Contributions, norder=2)
  f_AmtCont_tx_90[marker]  <- predict(spline_temp, 90)
  
  marker <- marker + 1
}

f_90 <- fund_model90 %>% group_by(ID) %>% 
  summarise(soci = max(Social_Network_Size),goal=max(Goal))

DM90 <- cbind( y      = outcome90,
                numcount = f_NumCont_tx_90, 
                amtcount   = f_AmtCont_tx_90, 
                goal   = f_90[,3],
                soci   = f_90[,2]) 
GAM90 <- lm(y ~ numcount+amtcount+goal+soci, data=as.data.frame(DM90[c(1:518),]))
summary(GAM90)

AIC(GAM90)
BIC(GAM90)
# calculate the mape of GAM90
### predict 90 percent of campaign with 519-618 obs ###
new <- DM90[c(519:618),]
pred <- predict(GAM90, new, se.fit = TRUE)
actual <- LR_dat90[c(519:618),]$contri
data_mape <- cbind(yhat=pred$fit,y=actual) %>% as.data.frame() %>% filter(pred$fit>0 & actual >0) 
GAM90.mape <- mape(data_mape$y,data_mape$yhat)



### Fig 8 ####
### R Square ###
R10 <- c(summary(LR10)["adj.r.squared"],summary(GAM10)["adj.r.squared"],
         summary(FDA10)["adj.r.squared"],summary(FDACOV10)["adj.r.squared"])
R30 <- c(summary(LR30)["adj.r.squared"],summary(GAM30)["adj.r.squared"],
         summary(FDA30)["adj.r.squared"],summary(FDACOV30)["adj.r.squared"])
R50 <- c(summary(LR50)["adj.r.squared"],summary(GAM50)["adj.r.squared"],
         summary(FDA50)["adj.r.squared"],summary(FDACOV50)["adj.r.squared"])
R70 <- c(summary(LR70)["adj.r.squared"],summary(GAM70)["adj.r.squared"],
         summary(FDA70)["adj.r.squared"],summary(FDACOV70)["adj.r.squared"])
R90 <- c(summary(LR90)["adj.r.squared"],summary(GAM90)["adj.r.squared"],
         summary(FDA90)["adj.r.squared"],summary(FDACOV90)["adj.r.squared"])

Time <- c(10,30,50,70,90)

Rdata <- rbind(unlist(R10),unlist(R30),unlist(R50),unlist(R70),unlist(R90))
Rdata <- cbind(Rdata,unlist(Time))
colnames(Rdata) <- c("LR","GAM","FDA","FDACovs","Time")
Rdata <- as.data.frame(Rdata)
Rdata
ggplot(Rdata, aes(x = Time)) + 
  geom_line(aes(y = LR), colour="blue") + 
  geom_line(aes(y = GAM), colour = "grey") + 
  geom_line(aes(y = FDA), colour = "red") + 
  geom_line(aes(y = FDACovs), colour = "orange") + 
  ylab(label="R-square value") + 
  xlab("Time to end of campaign")
# red line and orange line are overlapping

### AIC and BIC ###
AIC10 <- c(AIC(LR10),AIC(GAM10),
           AIC(FDA10),AIC(FDACOV10))
AIC30 <- c(AIC(LR30),AIC(GAM30),
         AIC(FDA30),AIC(FDACOV30))
AIC50 <- c(AIC(LR50),AIC(GAM50),
         AIC(FDA50),AIC(FDACOV50))
AIC70 <- c(AIC(LR70),AIC(GAM70),
         AIC(FDA70),AIC(FDACOV70))
AIC90 <- c(AIC(LR90),AIC(GAM90),
         AIC(FDA90),AIC(FDACOV90))

Time <- c(10,30,50,70,90)

AICdata <- rbind(unlist(AIC10),unlist(AIC30),unlist(AIC50),unlist(AIC70),unlist(AIC90))
AICdata <- cbind(AICdata,unlist(Time))
colnames(AICdata) <- c("LR","GAM","FDA","FDACovs","Time")
AICdata <- as.data.frame(AICdata)

ggplot(AICdata, aes(x = Time)) + 
  geom_line(aes(y = LR), colour="blue") + 
  geom_line(aes(y = GAM), colour = "grey") + 
  geom_line(aes(y = FDA), colour = "red") + 
  geom_line(aes(y = FDACovs), colour = "orange") + 
  ylab(label="AIC") + 
  xlab("Time to end of campaign")
# red line and orange line are overlapping


BIC10 <- c(BIC(LR10),BIC(GAM10),
           BIC(FDA10),BIC(FDACOV10))
BIC30 <- c(BIC(LR30),BIC(GAM30),
           BIC(FDA30),BIC(FDACOV30))
BIC50 <- c(BIC(LR50),BIC(GAM50),
           BIC(FDA50),BIC(FDACOV50))
BIC70 <- c(BIC(LR70),BIC(GAM70),
           BIC(FDA70),BIC(FDACOV70))
BIC90 <- c(BIC(LR90),BIC(GAM90),
           BIC(FDA90),BIC(FDACOV90))

Time <- c(10,30,50,70,90)

BICdata <- rbind(unlist(BIC10),unlist(BIC30),unlist(BIC50),unlist(BIC70),unlist(BIC90))
BICdata <- cbind(BICdata,unlist(Time))
colnames(BICdata) <- c("LR","GAM","FDA","FDACovs","Time")
BICdata <- as.data.frame(BICdata)

ggplot(BICdata, aes(x = Time)) + 
  geom_line(aes(y = LR), colour="blue") + 
  geom_line(aes(y = GAM), colour = "grey") + 
  geom_line(aes(y = FDA), colour = "red") + 
  geom_line(aes(y = FDACovs), colour = "orange") + 
  ylab(label="BIC") + 
  xlab("Time to end of campaign")
# red line and orange line are overlapping

###################
####  PREDICTION ###
###################
### show the mape results and the plot
mape10 <- c(LR10.mape,GAM10.mape,
           FDA10.mape,FDACOV10.mape)
mape30 <- c(LR30.mape,GAM30.mape,
            FDA30.mape,FDACOV30.mape)
mape50 <- c(LR50.mape,GAM50.mape,
            FDA50.mape,FDACOV50.mape)
mape70 <- c(LR70.mape,GAM70.mape,
            FDA70.mape,FDACOV70.mape)
mape90 <- c(LR90.mape,GAM90.mape,
            FDA90.mape,FDACOV90.mape)

Time <- c(10,30,50,70,90)

mapedata <- rbind(unlist(mape10),unlist(mape30),unlist(mape50),unlist(mape70),unlist(mape90))
mapedata <- cbind(mapedata,unlist(Time))
colnames(mapedata) <- c("LR","GAM","FDA","FDACovs","Time")
mapedata <- as.data.frame(mapedata)
### this is the table 3
head(mapedata)
### this is the fig9
ggplot(mapedata, aes(x = Time)) + 
  geom_line(aes(y = LR), colour="blue") + 
  geom_line(aes(y = GAM), colour = "black") + 
  geom_line(aes(y = FDA), colour = "red") + 
  geom_line(aes(y = FDACovs), colour = "orange") + 
  ylab(label="MAPE") + 
  xlab("Crowdfunding campaign time elapsed(%)")

### this is the fig10
fund_model_uncer <- fund_model %>% 
  filter(prog > 15 & prog < 30 & dura < 20) 

fund_model_un <- subset(fund_model,ID %in% fund_model_uncer$ID) 

length(unique(fund_model_un$ID))
ggplot(fund_model_un,aes(x=dura, y=prog, group=ID, color=as.character(Funding_Status))) +
    geom_smooth(span=1,se = FALSE) +
    ylim(0,150) +
  theme(legend.position="none")
  
