setwd("C:/STONY/Practice/R (No.5)/adoption/adoption")

##########
## No.1 ##
##########
### read the cluster data 20 times
cls <- list()
names_read <- paste0('cluster',1:20,'_edge','.csv')
for (i in 1:20){
  cls[[i]] <- read.csv(names_read[i],header = FALSE)
}

dim(cls[[1]])
### compute the group size
size <- list()
for (i in 1:20){
  a <- cls[[i]]
  size[[i]] <- dim(a)[1]
}

size = size %>% as.numeric()

### compute the network density of every group 
cls_dens <- function(x){
  col.num <- c()
  a <- dim(x)[2]
  for (i in 1:a){
    col.num[i] <- sum(x[,i])
  }
  num <- sum(col.num)
  density <- num/a%*%a
  return(density) 
}

density <- list()
for (i in 1:20){
  a <- cls[[i]]
  density[[i]] <- cls_dens(a)
}

density = density %>% as.numeric()

### compute the network coeffient
library(igraph)
library(dplyr)

cls_coeff <- list()
for (i in 1:20){
  a <- cls[[i]] %>% as.matrix()
  cls_coeff[[i]] <- graph_from_adjacency_matrix(a, mode = 'undirected') %>%
    transitivity(type = 'undirected')
}
cls_coeff = cls_coeff %>% as.numeric()

### generate a new dataframe
group <- c(1:20)
all_grp <- cbind(size,density,cls_coeff,group) %>% as.data.frame()
### figure out the size and the group which has highest density or coefficient
ggplot(all_grp,aes(group,size)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("The size of different groups")

### group 5 hsa the highest density
ggplot(all_grp,aes(group,density)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("The density of different groups")

### group 12 has the highest clustering coefficient
ggplot(all_grp,aes(group,cls_coeff)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("The clustering coeffient of different groups")

##########
## No.2 ##
##########
### read the sample data 20 times
sample <- list()
names_read1 <- paste0('sample',1:20,'.csv')
for (i in 1:20){
  sample[[i]] <- read.csv(names_read1[i],header = TRUE)
}

dim(sample[[1]])

adoption <- list()
for (i in 1:20){
  a <- sample[[i]]
  adoption[[i]] <- sum(a[,1])
}

adoption <- adoption %>% as.numeric()

### generate the new dataframe
grp_ado_net <-cbind(group,size,density,cls_coeff,adoption) %>% as.data.frame()
str(grp_ado_net)

model <- {adoption~size+density+cls_coeff}
fit <- lm(model,data=grp_ado_net)
summary(fit)
anova(fit,test="Chisq")
step(fit,direction="both")

set.seed(1234)
library(randomForest)
bk.rf.fit <- randomForest(model,data=grp_ado_net,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(bk.rf.fit,color="blue",pch=20,cex=1.25,main="")

##########
## No.3 ##
##########
### here we choose the data of group 3
cls_3 <- cls[[3]] %>% as.matrix()
sample_3 <- sample[[3]] %>% as.matrix()

net_3 <- list()
for (i in 1:105){
  net_3[[i]] <- sum(cls_3[i,])
}
net_3 <- net_3 %>% as.numeric()

data_3 <- cbind(sample_3,net_3) %>% as.data.frame()

## plot the network distribution #
## generate the adjacency matirx
m=as.matrix(cls_3)
igraph=graph_from_adjacency_matrix(m,mode="undirected",weighted=NULL,diag=FALSE)

## define the color 
color<-sample[[3]][,1]
b <- data.frame(ado=color)
V(igraph)$name <- c(1:105)
V(igraph)$ado=as.character(b[V(igraph)$name,])
head(V(igraph)$ado)
V(igraph)$color=V(igraph)$ado
V(igraph)$color=gsub("0","red",V(igraph)$color) #0 will be red
V(igraph)$color=gsub("1","blue",V(igraph)$color) #1 will be blue
plot.igraph(igraph,vertex.label=NA,layout=layout.fruchterman.reingold)

## define the size
set.seed(123)
V(igraph)$size=degree(igraph)*2 
plot.igraph(igraph,main="network",vertex.label=NA,layout=layout.fruchterman.reingold)


##########
## No.4 ##
##########
### degree centrality ###
data_3 <- cbind(sample_3,net_3) %>% as.data.frame()
#data_3$adoption %>% as.factor()
model <- {adoption~net_3+smart+gender}
fit <- glm(model,family=binomial,data=data_3)
summary(fit)
anova(fit,test="Chisq")
step(fit,direction="both")

set.seed(1234)
bk.rf.fit <- randomForest(model,data=data_3,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(bk.rf.fit,color="blue",pch=20,cex=1.25,main="")


##########
## No.5 ##
##########
#####  LOGISTIC REGRESSION #####
### get the train data
## get the centrality of whole 20 groups
a <- list()
net_20 <- list()
for (i in 1:20){
  a[[i]] <- cls[[i]] %>% as.matrix()
  b <- list()
  for (j in 1:dim(a[[i]])[1]){
    b[[j]] <- sum(a[[i]][j,])
    b <- b %>% as.data.frame()
  }
  net_20[[i]] <- b
  #net_20[[i]] <-  net_20[[i]] %>% as.data.frame() 
  net_20[[i]] <-  t(net_20[[i]]) %>% as.numeric()  %>% as.matrix()
}

for (i in 1:20){
  net_20[[i]] <- net_20[[i]] %>% as.numeric()
}
train <- unlist(net_20) %>% as.matrix() %>% as.data.frame()
### get the train data by combining the 20 groups
a <- bind_rows(sample)
train20 <- cbind(a,train)
str(train20)

traindata <- train20[1:1747,]
traindata$adoption %>% as.factor()
model <- {adoption ~ gender+age+smart+V1}
traindata.lr.fit <- glm(model,family=binomial,data=traindata)

# area under ROC curve for TRAINING data
library(lattice)      # lattice plot
library(vcd)          # mosaic plots
library(ROCR)         # ROC curve objects for binary classification

traindata$lr.predprob <- predict(traindata.lr.fit,type="response")
traindata.lr.pred <- prediction(traindata$lr.predprob,traindata$adoption)
traindata.lr.auc <- as.numeric(performance(traindata.lr.pred,"auc")@y.values)

# area under ROC curve for TEST data

testdata <- train20[1748:2621,]
testdata$lr.predprob <- as.numeric(predict(traindata.lr.fit,
                                             testdata,type="response"))
testdata.lr.pred <- prediction(testdata$lr.predprob,testdata$adoption)
testdata.lr.auc <- as.numeric(performance(testdata.lr.pred,"auc")@y.values)

# genertae a function to plot the ROC
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
# ROC for logistic regression to know the accuracy

traindata.lr.roc <- performance(traindata.lr.pred,"tpr","fpr")
testdata.lr.roc <- performance(testdata.lr.pred,"tpr","fpr")
plot.roc(train.roc=traindata.lr.roc,train.auc=traindata.lr.auc, 
         test.roc=testdata.lr.roc,test.auc=testdata.lr.auc)


### get the prediction data
## get the cluster data of 20 groups
setwd("C:/STONY/Practice/R (No.5)/prediction/prediction")
pred <- list()
names_read2 <- paste0('prediction',21:40,'.csv')
for (i in 1:20){
  pred[[i]] <- read.csv(names_read2[i],header = TRUE)
}


pred_cls <- list()
names_read3 <- paste0('cluster',21:40,'_edge','.csv')
names_read3[1]
for (i in 1:20){
  pred_cls[[i]] <- read.csv(names_read3[i],header = FALSE)
}


## get the centrality of group 21-40
pre_cen <- list()
net_40 <- list()
for (i in 1:20){
  pre_cen[[i]] <- pred_cls[[i]] %>% as.matrix()
  b <- list()
  for (j in 1:dim(pre_cen[[i]])[1]){
    b[[j]] <- sum(pre_cen[[i]][j,])
    b <- b %>% as.data.frame()
  }
  net_40[[i]] <- b
  net_40[[i]] <-  t(net_40[[i]]) %>% as.numeric()  %>% as.matrix()
}

for (i in 1:20){
  net_40[[i]] <- net_40[[i]] %>% as.numeric()
}

### get the train data by combining the 20 groups
pred_40_c <- list()
pred_40_s <- list()
k <- list()
for (i in 1:20){
  pred_40_c[[i]] <- net_40[[i]]
  pred_40_s[[i]] <- pred[[i]]
  k[[i]] <- cbind(pred_40_s[[i]],V1=pred_40_c[[i]])
}

#### PREDICTION !
adop <- c()
h <- c()
for (i in 1:20){
  for (j in 1:dim(k[[i]])[1]){
    h[j] <- predict(traindata.lr.fit,newdata=k[[i]][j,],type="response")
    if(h[j] >= 0.35){
      h[j] = 1
    }
    else{
      h[j] = 0
    }
  }
  adop[[i]] <- sum(h)
}
adop

rank(adop)
## we can see group 39 has the biggest number of adoption
## and the top 10 groups are 7-13,16,19-20

