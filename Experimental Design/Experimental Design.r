# set up the environment where you get and use your data
setwd("C:/STONY/Practice/R/R (No.18)")
library(ggplot2)

### Q2 ###
# read the data 
data = read.csv('Teachingexample (1).csv')
str(data)

# have a quick look at the data by plotting
ggplot(data=data, aes(x=TeachMethod, y=Score))+
  geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=1,binwidth=0.5)
# we can see there are big differences in score among teach method

# build a linear regression model to have a check
model <- lm(Score~TeachMethod,data=data)
round(summary(model)$coefficients[,4],3) # method B is most significant when compare with A

# to see which factor has significant effect on score
a1 <- aov(data$Score~data$TeachMethod)
summary(a1)
TukeyHSD(x=a1, 'data$TeachMethod', conf.level=0.95)
# B-A 0.0831363 and D-B 0.0137874 are significant , while others are not significant. 

## plot a1
plot(a1)

sd(data$Score)

### Q2 ###
# 1 #
aggregate(Score ~ TeachMethod,FUN=mean,data=data)
  #TeachMethod    Score
#1           A 63.00000
#2           B 71.77778
#3           C 66.00000
#4           D 60.55556
#5           E 64.77778
# From the Tukey test. B-A and D-B are significant , while others are not significant. 
# method B is most significant when compare with A


# 2-4 #
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
TeachMethod <- rep(c("A","B","C","D","E"),each=9)
treatmentmeans <- c(63,72,66,61,65)
Score <- round(rnorm(45,rep(treatmentmeans,each=9),7.7))
var(Score) 
var(data$Score) # they are comparable

# 5 #
# with and without blockings
Group <- c()
for(i in 1:5){
  Group[[i]] <- sample(c(1:9),size=9,replace =F)
}
Group <- unlist(Group)
data_simu <- data.frame(cbind(Group=Group,TeachMethod,Score=Score))
data_simu$Score <- as.numeric(as.character(data_simu$Score))
summary(lm(Score~TeachMethod+Group,data=data_simu))

### analysis anova models
a2 <- aov(data_simu$Score~data_simu$TeachMethod)
summary(a2)
a3 <- aov(data_simu$Score~data_simu$TeachMethod + data_simu$Group)
summary(a3)

# 6 # 
summary(a2)[[1]][[1,"Pr(>F)"]]
summary(a3)[[1]][[1,"Pr(>F)"]]
# they are both significant cause they are smaller than 0.05

# 7 #
Model1 <- lm(Score~factor(TeachMethod), data=data_simu)
Model2<-lm(Score~factor(TeachMethod)+factor(Group),data=data_simu)
summary(Model1)
summary(Model2)

set.seed(551347264, kind = "Mersenne-Twister", normal.kind = "Inversion")

# simulate new data frame for 1000 times

pvalexample <- cbind(rep(NA,N),rep(NA,N))
sigma <- summary(Model2)$"sigma"

for( i in 1:N){
  
  Scores <- fitted(Model2)+rnorm(45,0,sigma)
  test_dat <- data.frame(Score = Scores,TeachMethod=TeachMethod,Group = Group,row.names = 1:45)
  
  pvalexample[i,1] <- anova(lm(Score~factor(TeachMethod), data=test_dat))$"Pr(>F)"[1]
  
  pvalexample[i,2] <- anova(lm(Score~factor(TeachMethod)+factor(Group), data=test_dat))$"Pr(>F)"[1]
  
}
a <- list(table(I(pvalexample[,1]<0.05)),table(I(pvalexample[,2]<0.05)))
a
results <- list(mean(pvalexample[,1]<0.05),mean(pvalexample[,2]<0.05))
results #[1] 0.895 [2] 0.954



### Q3 ###  <<< Method 1>>>
## true effects are the half size 
Model3 <-lm(Score~factor(TeachMethod)+factor(Group),data=data_simu)
summary(Model3)
anova(Model3)
# here we set the coefficient of our new model as half of former ones
for(i in 1:13){
  Model3$coefficients[i] <- (summary(Model3)$coefficients[i,1])/2
}
summary(Model3)
anova(Model3)
###### P-value of the anova test is the same with the former model3 without change in coefficients
###### so maybe this method is wrong.There is no difference in p-value.

### But we can still have a try to see how the power changes 
set.seed(551347264, kind = "Mersenne-Twister", normal.kind = "Inversion")
# simulate new data frame for 1000 times
Scores <-c()
results <- c()
proportion <- c()
sigma <- summary(Model3)$"sigma"

# here we implemente a for loop(j in 1:10) which generates 45*j records using model3' sigma respectively
# Thus we can have different sample size to check the power trajectory
for(j in 1:30){
  N <- 100
  pvalexample <- rep(NA,N)
  T <- rep(c("A","B","C","D","E"),each=9*j)
  size <- 9*j
  Group <- c()
  for(k in 1:5){
    Group[[k]] <- sample(c(1:size),size=size,replace =F)
  }
  Group <- unlist(Group) 
  Scores <- c(Scores,fitted(Model3))
  
  for( i in 1:N){
    
    Scores <- Scores+rnorm(45*j,0,sigma)
    test_dat <- data.frame(Score = Scores,TeachMethod=T,Group = Group)
    
    
    pvalexample[i] <- anova(lm(Score~factor(TeachMethod)+factor(Group), data=test_dat))$"Pr(>F)"[1]
    
  }
  proportion <- c(proportion,table(I(pvalexample<0.1)))
  results <- c(results,mean(pvalexample<0.1))
}

results
proportion
# very time-comsuming


### Q3 ###  <<< Method 2 >>>
## true effects are the half size..  means we need to delete two of the treatments
DT <- data
DT <- DT[-which(DT$TeachMethod=='C'),]
DT <- DT[-which(DT$TeachMethod=='E'),]
Model3 <-lm(Score~factor(TeachMethod)+factor(Group),data=DT)
summary(Model3)
anova(Model3) # p-value gets larger 

# simulate new data frame nly keeping 2 treatments 
set.seed(551347264, kind = "Mersenne-Twister", normal.kind = "Inversion")
T <- rep(c("A","B","D"),each=9)
Group <- c()
for(k in 1:3){
  Group[[k]] <- sample(c(1:9),size=9,replace =F)
}
Group <- unlist(Group) 

# here when we do the loop, each loop has 27 more records than the former one,
# so here, with the increase in N times, our sample size also increases in the meantime
proportion <- c()
results <- c()
sigma <- summary(Model3)$"sigma"
  for(N in 1:500){
    pvalexample <- rep(0,N)
  for( i in 1:N){
    Scores <- fitted(Model3)+rnorm(27,0,sigma)
    test_dat <- data.frame(Score = Scores,TeachMethod=T,Group = Group)
    pvalexample[i] <- anova(lm(Score~factor(TeachMethod)+factor(Group), data=test_dat))$"Pr(>F)"[1]
  }
  
  proportion <- c(proportion,table(I(pvalexample<0.1)))
  results <- c(results,mean(pvalexample<0.1))
  }
proportion
results

which(results>0.9) # 51

# when N gets larger, it will be very time-consuming. 
# And the sample size should be > N*27
