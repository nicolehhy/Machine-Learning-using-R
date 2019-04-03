  setwd("C:/STONY/Practice/R(No.4)")

#install.packages("ROCR")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("nnet")

  library(lattice)      # lattice plot
  library(vcd)          # mosaic plots
  library(e1071)        # support vector machines
  library(randomForest) # random forests
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

#########################################
## STEP I CLEANING AND SUBSETTING DATA ##
#########################################

  banking.df <- read.csv("online_banking.csv")

  str(banking.df)
  dim(banking.df)
  head(banking.df)
  summary(banking.df)

  banking.df$age.cat <- ifelse(banking.df$age==1,"<15",
                          ifelse(banking.df$age==2,"15-24",
                            ifelse(banking.df$age==3,"25-34",
                              ifelse(banking.df$age==4,"35-44",
                                ifelse(banking.df$age==5,"45-54",
                                  ifelse(banking.df$age==6,"55-64",">65"))))))
  banking.df$age.cat <- factor(banking.df$age.cat,
                               level=c("<15","15-24","25-34","35-44","45-54","55-64",">65"))

  banking.df$income.cat <- ifelse(banking.df$income==1,"<15K",
                             ifelse(banking.df$income==2,"15K-20K",
                               ifelse(banking.df$income==3,"20K-30K",
                                 ifelse(banking.df$income==4,"30K-40K",
                                   ifelse(banking.df$income==5,"40K-50K",
                                     ifelse(banking.df$income==6,"50K-75K",
                                       ifelse(banking.df$income==7,"75K-100K",
                                         ifelse(banking.df$income==8,"100K-125K",">125K"))))))))
  banking.df$income.cat <- factor(banking.df$income.cat,
                                  levels=c("<15K","15K-20K","20K-30K","30K-40K","40K-50K","50K-75K","75K-100K","100K-125K",">125K"))

  banking.df$district <- factor(ifelse(banking.df$district==1,"Texas Panhandle",
                                  ifelse(banking.df$district==2,"Northeast Texas","North Central Texas")),
                                levels=c("Texas Panhandle","Northeast Texas","North Central Texas"))

  banking.df$online15.cat <- ifelse(banking.df$online15==0,"No","Yes")
  banking.df$online15.cat <- factor(banking.df$online15.cat,levels=c("No","Yes"))

  banking.df$billpay15.cat <- ifelse(banking.df$billpay15==0,"No","Yes")
  banking.df$billpay15.cat <- factor(banking.df$billpay15.cat,levels=c("No","Yes"))

  banking.df$retain <- ifelse(is.na(banking.df$profit16)==FALSE,"Yes","No")
  banking.df$retain <- factor(banking.df$retain,levels=c("No","Yes"),
                              labels=c("Other Bank","Pilgrim Bank"))

  str(banking.df)
  dim(banking.df)
  head(banking.df)
  summary(banking.df,maxsum=10)

  banking.use.df <- banking.df[,c(15,11,12,3:5,13,14)]
  colnames(banking.use.df) <- c("retain","age","income","tenure","district",
                                "profit15","online15","billpay15")
  str(banking.use.df)
  dim(banking.use.df)
  head(banking.use.df)
  summary(banking.use.df,maxsum=10)

  bk.df <- banking.use.df

#################################
## STEP II VARIABLE IMPORTANCE ##
#################################

# !! THIS PART HELPS WITH QUESTION 1 !!

  model <- {retain~age+income+tenure+district+profit15+online15+billpay15}
  fit <- glm(model,family=binomial,data=bk.df)
  summary(fit)
  anova(fit,test="Chisq")
  step(fit,direction="both")

  set.seed(1234)
  bk.rf.fit <- randomForest(model,data=bk.df,mtry=3,ntree=1000,importance=TRUE)
  varImpPlot(bk.rf.fit,color="blue",pch=20,cex=1.25,main="")
# importance(bk.rf.fit)

########################################
## STEP III TRAINING-AND-TEST REGIMEN ##
########################################

# !! THIS PART HELPS WITH QUESTION 2 !!

  set.seed(1234)
  partition <- sample(nrow(bk.df),replace=FALSE)
  bk.df$group <- ifelse(partition<(2/3)*nrow(bk.df),1,2)
  bk.df$group <- factor(bk.df$group,levels=c(1,2),labels=c("TRAIN","TEST"))
  bk.train.df <- subset(bk.df,subset=(group=="TRAIN"), 
                        select=c("retain","age","income","tenure","district",
                                 "profit15","online15","billpay15"))
  bk.test.df <-  subset(bk.df,subset=(group=="TEST"), 
                        select=c("retain","age","income","tenure","district",
                                 "profit15","online15","billpay15"))
  bk.train.df <- na.omit(bk.train.df)
  bk.test.df <- na.omit(bk.test.df)
  if(length(intersect(rownames(bk.train.df),rownames(bk.test.df)))!= 0) {
     print("\nProblem with partition")
     }

###############################
## STEP IV MODEL CALIBRATION ##
##           AND             ##
## STEP V MODEL VALIDATION   ##
###############################

# !! THIS PART HELPS WITH QUESTION 2 !!

##### [1] LOGISTIC REGRESSION #####

  bk.train.lr.fit <- glm(model,family=binomial,data=bk.train.df)

# area under ROC curve for TRAINING data

  bk.train.df$lr.predprob <- predict(bk.train.lr.fit,type="response")
  bk.train.lr.pred <- prediction(bk.train.df$lr.predprob,bk.train.df$retain)
  bk.train.lr.auc <- as.numeric(performance(bk.train.lr.pred,"auc")@y.values)

# area under ROC curve for TEST data

  bk.test.df$lr.predprob <- as.numeric(predict(bk.train.lr.fit,
                                       newdata=bk.test.df,type="response"))
  bk.test.lr.pred <- prediction(bk.test.df$lr.predprob,bk.test.df$retain)
  bk.test.lr.auc <- as.numeric(performance(bk.test.lr.pred,"auc")@y.values)

# ROC for logistic regression

  bk.train.lr.roc <- performance(bk.train.lr.pred,"tpr","fpr")
  bk.test.lr.roc <- performance(bk.test.lr.pred,"tpr","fpr")
  plot.roc(train.roc=bk.train.lr.roc,train.auc=bk.train.lr.auc, 
           test.roc=bk.test.lr.roc,test.auc=bk.test.lr.auc)

##### [2] SUPPORT VECTOR MACHINES #####

# set.seed(1234)
# bk.trainsamp <- sample(1:dim(bk.train.df)[1],size=1000,replace=FALSE)
# bk.trainsamp.df <- bk.train.df[bk.trainsamp,]
# rownames(bk.trainsamp.df) <- NULL
#
# set.seed(1234)
# bk.train.svm.tune <- tune(svm,model,data=bk.trainsamp.df,
#                           ranges=list(gamma=2^(-16:1),cost=2^(0:8)),
#                           tunecontrol=tune.control(sampling="fix"))
#
# set.seed(1234)
# bk.train.svm.fit <- svm(model,data=bk.train.df, 
#                         cost=bk.train.svm.tune$best.parameters$cost, 
#                         gamma=bk.train.svm.tune$best.parameters$gamma, 
#                         probability=TRUE)

  set.seed(1234)
  bk.train.svm.fit <- svm(model,data=bk.train.df,cost=1,gamma=1.525879e-05, 
                          probability=TRUE)

# area under ROC curve for TRAINING data

  bk.train.svm.predict <- predict(bk.train.svm.fit,bk.train.df,probability=TRUE)
  bk.train.df$svm.predprob <- attr(bk.train.svm.predict,"probabilities")[,1]
  bk.train.svm.prediction <- prediction(bk.train.df$svm.predprob,bk.train.df$retain)
  bk.train.svm.auc <- as.numeric(performance(bk.train.svm.prediction,"auc")@y.values)

# area under ROC curve for TEST data

  bk.test.svm.predict <- predict(bk.train.svm.fit,bk.test.df,probability=TRUE)
  bk.test.df$svm.predprob <- attr(bk.test.svm.predict,"probabilities")[,1]
  bk.test.svm.prediction <- prediction(bk.test.df$svm.predprob,bk.test.df$retain)
  bk.test.svm.auc <- as.numeric(performance(bk.test.svm.prediction,"auc")@y.values)

# ROC for support vector machines classification

  bk.train.svm.roc <- performance(bk.train.svm.prediction,"tpr","fpr")
  bk.test.svm.roc <- performance(bk.test.svm.prediction,"tpr","fpr")
  plot.roc(train.roc=bk.train.svm.roc,train.auc=bk.train.svm.auc, 
           test.roc=bk.test.svm.roc,test.auc=bk.test.svm.auc)   

##### [3] NEURAL NETWORKS #####

  set.seed(1234)
  bk.train.nnet.fit <- nnet(model,data=bk.train.df,size=3,decay=0,
                            probability=TRUE,trace=FALSE) 

# area under ROC curve for TRAINING data

  bk.train.df$nnet.predprob <- as.numeric(predict(bk.train.nnet.fit,newdata=bk.train.df))
  bk.train.nnet.prediction <- prediction(bk.train.df$nnet.predprob,bk.train.df$retain)
  bk.train.nnet.auc <- as.numeric(performance(bk.train.nnet.prediction,"auc")@y.values)

# area under ROC curve for TEST data

  bk.test.df$nnet.predprob <- as.numeric(predict(bk.train.nnet.fit,newdata=bk.test.df))
  bk.test.nnet.prediction <- prediction(bk.test.df$nnet.predprob,bk.test.df$retain)
  bk.test.nnet.auc <- as.numeric(performance(bk.test.nnet.prediction,"auc")@y.values)
 
# ROC for neural network classification

  bk.train.nnet.roc <- performance(bk.train.nnet.prediction,"tpr","fpr")
  bk.test.nnet.roc <- performance(bk.test.nnet.prediction,"tpr","fpr")
  plot.roc(train.roc=bk.train.nnet.roc,train.auc=bk.train.nnet.auc, 
           test.roc=bk.test.nnet.roc,test.auc=bk.test.nnet.auc)

##### [4] NAIVE BAYES #####

  set.seed(1234)
  bk.train.nb.fit <- naiveBayes(model,data=bk.train.df)

# area under ROC curve for TRAINING data

  bk.train.df$nb.predprob <- as.numeric(predict(bk.train.nb.fit,
                                                newdata=bk.train.df,
                                                type="raw")[,2])
  bk.train.nb.prediction <- prediction(bk.train.df$nb.predprob,bk.train.df$retain)
  bk.train.nb.auc <- as.numeric(performance(bk.train.nb.prediction,"auc")@y.values)

# area under ROC curve for TEST data

  bk.test.df$nb.predprob <- as.numeric(predict(bk.train.nb.fit,
                                               newdata=bk.test.df, 
                                               type="raw")[,2])
  bk.test.nb.prediction <- prediction(bk.test.df$nb.predprob,bk.test.df$retain)
  bk.test.nb.auc <- as.numeric(performance(bk.test.nb.prediction,"auc")@y.values)

# ROC for naive Bayes classification

  bk.train.nb.roc <- performance(bk.train.nb.prediction,"tpr","fpr")
  bk.test.nb.roc <- performance(bk.test.nb.prediction,"tpr","fpr")
  plot.roc(train.roc=bk.train.nb.roc,train.auc=bk.train.nb.auc, 
           test.roc=bk.test.nb.roc,test.auc=bk.test.nb.auc)

##### [5] RANDOM FORESTS #####

  set.seed(1234)
  bk.train.rf.fit <- randomForest(model,data=bk.train.df,mtry=3,
                                  importance=FALSE,ntree=1000)

# area under ROC curve for TRAINING data

  bk.train.df$rf.predprob <- as.numeric(predict(bk.train.rf.fit,type="prob")[,2])
  bk.train.rf.prediction <- prediction(bk.train.df$rf.predprob,bk.train.df$retain)
  bk.train.rf.auc <- as.numeric(performance(bk.train.rf.prediction,"auc")@y.values)

# area under ROC curve for TEST data

  bk.test.df$rf.predprob <- as.numeric(predict(bk.train.rf.fit,newdata=bk.test.df,
                                                type="prob")[,2])
  bk.test.rf.prediction <- prediction(bk.test.df$rf.predprob,bk.test.df$retain)
  bk.test.rf.auc <- as.numeric(performance(bk.test.rf.prediction,"auc")@y.values)

# ROC for random forest classification

  bk.train.rf.roc <- performance(bk.train.rf.prediction,"tpr","fpr")
  bk.test.rf.roc <- performance(bk.test.rf.prediction,"tpr","fpr")
  plot.roc(train.roc=bk.train.rf.roc,train.auc=bk.train.rf.auc, 
           test.roc=bk.test.rf.roc,test.auc=bk.test.rf.auc)

######################################
## SUMMARY OF AREA UNDER ROC CURVES ##
######################################

# !! THIS PART HELPS WITH QUESTION 2 !!

  rbind(bk.train.lr.auc,
        bk.train.svm.auc,
        bk.train.nnet.auc,
        bk.train.nb.auc,
        bk.train.rf.auc)

  rbind(bk.test.lr.auc,
        bk.test.svm.auc,
        bk.test.nnet.auc,
        bk.test.nb.auc,
        bk.test.rf.auc)

################################
## PREDICTION OF NEW CUSTOMER ##
################################

# !! THIS PART HELPS WITH QUESTION 3 !!

  bk_new.df <- data.frame(age="15-24",income="50K-75K",tenure=0,district="Northeast Texas",
                          profit15=0,online15="No",billpay15="No")
  bk_new.df$nnet.predprob <- as.numeric(predict(bk.train.nnet.fit,newdata=bk_new.df,type="raw"))
  bk_new.df$nnet.predYN <- predict(bk.train.nnet.fit,newdata=bk_new.df,type="class")
  