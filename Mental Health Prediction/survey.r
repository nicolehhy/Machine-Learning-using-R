setwd("C:/STONY/Practice/R(No.4)")

# load libraries
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
# read the data
survey <- read.csv("C:/STONY/Practice/R (No.10)/survey.csv",header = T)
# What does the dataset look like
head(survey)

#################
# Data Cleaning #
#################
# the survey data is a raw data set, so we have to clean it 
# categorize gender as female, male, undecided
survey$s<- ifelse(survey$Gender %in% c("F" , "Female", "FEMALE" ,"female", "f"), "Female" ,
                  ifelse(survey$Gender %in% c("Male", "male","M","m","MALE","maile"), "Male", "Undecided"))

# categorize treatment score as 1(yes),0(no)
survey$treats <- ifelse(survey$treatment=="Yes", 1,0)

# categorize work interfere as 1(yes),0(no)
survey$worki <- ifelse(is.na(survey$work_interfere), "0", survey$work_interfere)


# categorize company size as small,meduim and large
survey$cpsize<- ifelse(survey$no_employees %in% c("1-5" , "6-25", "26-100"), "Small",
                       ifelse(survey$no_employees %in% c("100-500","500-1000"), "Meduim", "Large"))

#####################
# Data Visulization #
#####################
## NO.1---the basic respondents information visualization ##

# plot the age distribution
age_plot <- ggplot(survey, aes(Age))+
  geom_histogram()+xlim(0,75)+labs(title="Age Distribution")
age_plot

# plot the gender distribution
gend_plot <- ggplot(survey, aes(x=as.character(s)))+
  geom_bar()+labs(title="Gender Distribution")
gend_plot

# plot the company size distribution
#Company size
cmp_plot <- ggplot(survey, aes(x=cpsize))+
  geom_bar(fill="#62AB61")+
  labs(x="Company size", y="Count",
       title="Company Size Distribution")+ theme(legend.position="none") 
cmp_plot


## NO.2---the basic mental health information visualization ##

# plot the Probability of mental health illness by workplace type and size
ggplot(survey,aes(x=cpsize,y=treats, fill=factor(tech_company)), color=factor(vs)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar") + 
  labs(x = "Number of employees", y = "Probability of mental health condition", 
       title = "Probability of mental health illness by workplace type and size")

# plot the probablity of mental health illness by family history and anonymity
ggplot(survey,aes(x=family_history,y=treats, fill=factor(anonymity))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar") + 
  labs(x = "Family History", y = "Probability of mental health condition", 
       title = "Probability of mental health illness by family history and anonymity")

# plot the probablity of mental health illness by benefits and care_options
ggplot(survey,aes(x=benefits,y=treats, fill=factor(care_options))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar") + 
  labs(x = "Family History", y = "Probability of mental health condition", 
       title = "Probability of mental health illness by benefits and care_options")

# plot the worldwide mental treatment consideration distribution
# first calculate the percentage of each conutry poeple thinking about treat
install.packages("rworldmap")
library(rworldmap)
icountry <- group_by(survey, Country)
ic2 <- dplyr::summarise(icountry, add=sum(treats,na.rm=TRUE) , n=n())
ic2$treatpct <- ((ic2$add*100)/ (ic2$n))
ic2 <- arrange(ic2, desc(treatpct))

n <- joinCountryData2Map(ic2, joinCode="NAME", nameJoinColumn="Country")
mapCountryData(n, nameColumnToPlot="treatpct", 
               mapTitle="Worldwide Mental Treatment Consideration Distribution", 
               catMethod="fixedWidth", colourPalette = "rainbow")


#####################
# Prediction Model  #
#####################
# Preparing regression function for the use in other methods
regresion <- treats~
  s+
  family_history+
  worki+
  benefits+
  care_options+
  anonymity

# build the logistic regression for the model and check the variable importance
fit <- glm(regresion,family=binomial,data=survey)
summary(fit)
anova(fit,test="Chisq")
step(fit,direction="both")

# build the random forest model and double check the variable importance
set.seed(1234)
data_fac=survey %>% mutate_if(is.character, as.factor)
rf.fit <- randomForest(regresion,data=data_fac,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(rf.fit,color="blue",pch=20,cex=1.25,main="")



library(lattice)      # lattice plot
library(vcd)          # mosaic plots
library(nnet)         # neural networks
library(ROCR)         # ROC curve objects for binary classification

# user-defined function for plotting ROC curve using ROC objects from ROCR

plot.roc <- function(train.roc,train.auc,test.roc,test.auc) {
  plot(train.roc,col="blue",lty="solid",main="",lwd=2,
       xlab="False Positive Rate",ylab="True Positive Rate")
  plot(test.roc,col="red",lty="dashed",lwd=2,add=TRUE)
  abline(c(0,1))
  train.legend <- paste("Training AUC = ",round(train.auc,digits=3))
  test.legend <- paste("Test AUC = ",round(test.auc,digits=3))
  legend("bottomright",legend=c(train.legend,test.legend),
         lty=c("solid","dashed"),lwd=2,col=c("blue","red"))
}


# !! THIS PART HELPS WITH DEVIDING DATA INTO TRAIN AND TEST !!
set.seed(1234)
partition <- sample(nrow(survey_origin),replace=FALSE)
survey_origin$group <- ifelse(partition<(2/3)*nrow(survey_origin),1,2)
survey_origin$group <- factor(survey_origin$group,levels=c(1,2),labels=c("TRAIN","TEST"))
train.df <- subset(survey_origin,subset=(group=="TRAIN"), 
                      select=c("s","family_history","worki","benefits","care_options",
                               "anonymity","treats"))
test.df <-  subset(survey_origin,subset=(group=="TEST"), 
                   select=c("s","family_history","worki","benefits","care_options",
                            "anonymity","treats"))
train.df <- na.omit(train.df)
test.df <- na.omit(test.df)
if(length(intersect(rownames(train.df),rownames(test.df)))!= 0) {
  print("\nProblem with partition")
}

##### [1] LOGISTIC REGRESSION #####

train.lr.fit <- glm(regresion,family=binomial,data=train.df)

# area under ROC curve for TRAINING data

train.df$lr.predprob <- predict(train.lr.fit,type="response")
train.lr.pred <- prediction(train.df$lr.predprob,train.df$treats)
train.lr.auc <- as.numeric(performance(train.lr.pred,"auc")@y.values)

# area under ROC curve for TEST data

test.df$lr.predprob <- as.numeric(predict(train.lr.fit,
                                             newdata=test.df,type="response"))
test.lr.pred <- prediction(test.df$lr.predprob,test.df$treats)
test.lr.auc <- as.numeric(performance(test.lr.pred,"auc")@y.values)

# ROC for logistic regression

train.lr.roc <- performance(train.lr.pred,"tpr","fpr")
test.lr.roc <- performance(test.lr.pred,"tpr","fpr")
plot.roc(train.roc=train.lr.roc,train.auc=train.lr.auc, 
         test.roc=test.lr.roc,test.auc=test.lr.auc)

##### [2] RANDOM FORESTS #####

set.seed(1234)
train.nnet.fit <- nnet(regresion,data=train.df,size=3,decay=0,
                          probability=TRUE,trace=FALSE) 

# area under ROC curve for TRAINING data

train.df$nnet.predprob <- as.numeric(predict(train.nnet.fit,newdata=train.df))
train.nnet.prediction <- prediction(train.df$nnet.predprob,train.df$treats)
train.nnet.auc <- as.numeric(performance(train.nnet.prediction,"auc")@y.values)

# area under ROC curve for TEST data

test.df$nnet.predprob <- as.numeric(predict(train.nnet.fit,newdata=test.df))
test.nnet.prediction <- prediction(test.df$nnet.predprob,test.df$treats)
test.nnet.auc <- as.numeric(performance(test.nnet.prediction,"auc")@y.values)

# ROC for neural network classification

train.nnet.roc <- performance(train.nnet.prediction,"tpr","fpr")
test.nnet.roc <- performance(test.nnet.prediction,"tpr","fpr")
plot.roc(train.roc=train.nnet.roc,train.auc=train.nnet.auc, 
         test.roc=test.nnet.roc,test.auc=test.nnet.auc)

### logistic regression model outperforms the neural network


################################
## PREDICTION OF NEW CUSTOMER ##
################################

# !! THIS PART HELPS WITH PREDICTING A NEW RESPONDENT WHEATHER HE CONSIDER TREATS !!

# For example, there is a new respondent who is a woman,without family history
# considering mental illness interfere work sometimes, with benefits in workplace
# without care_options in company, and anonymity is yes. predict whether she considers
# mental health treats

new.df <- data.frame(s="Female",family_history="No",worki="3",benefits="Yes",
                     care_options="No",anonymity="Yes")
new.df$lr.predprob <- as.numeric(predict(train.lr.fit,newdata=new.df,type="response"))
new.df$lr.predYN <- predict(train.lr.fit,newdata=new.df,type="response")
new.df$lr.predYN 
## the result is 1, so the woman will consider about mental health treat
