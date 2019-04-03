str(review_sample)

### calculate positive and negative
library(stringr)
#setup the working directory
setwd("C:/STONY/Practice/R (No.12)")
nk.text<- sapply(review_sample$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#loading Hu Liu's opinion lexicon
hu.liu.pos<- scan("positive-words.txt",what="character", comment.char="");
hu.liu.neg<- scan("negative-words.txt",what="character", comment.char="");

#loading some industry-specific and/or especially emphatic terms
pos.words<- c(hu.liu.pos, 'prize')
neg.words<- c(hu.liu.neg, 'late')

#function for the score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = (sum(pos.matches) - sum(neg.matches))-2
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


#score the tweets
nk.scores=score.sentiment(nk.text,pos.words,neg.words,.progress='text')
class(nk.scores)
#save the score in a csv file
write.csv(nk.scores, "review_scores.csv")

### read the sentiment information and create new data frame
sentiment <- read.csv("review_scores.csv")[,2]
longevity = difftime(as.Date("2016-12-31"), as.Date(review_sample$date), units="days")
review_len = log(length(review_sample$text))
reader = log(review_sample$votes_sum)*100
help = review_sample$votes$useful/(review_sample$votes_sum)
review <- cbind(sentiment,longevity,review_len,reader,help,star=review_sample$stars) %>% as.data.frame()


### scatter plot among the variables
plot(review, main="Scatter plot")

### correlation function among the width and length
cor <- cor(review) %>% as.matrix()
install.packages("corrplot")
library(corrplot)
corrplot(cor, method="circle") # the darker color is, the greater the correlation will be
corrplot(cor, method="number")


# Prediction Model  #
#####################
# Preparing regression function for helpness
regression <- help~
  star+
  sentiment+
  longevity+
  review_len
  

# build the regression for the model and check the variable importance
fit <- lm(regression,data=review)
summary(fit)
anova(fit,test="Chisq")

# build the random forest model and double check the variable importance
set.seed(1234)
library(randomForest)
data_fac=review %>% mutate_if(is.character, as.factor)
rf.fit <- randomForest(regression,data=data_fac,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(rf.fit,color="blue",pch=20,cex=1.25,main="")



# Preparing regression function for readership
regression <- reader~
  star+
  sentiment+
  longevity+
  review_len


# build the regression for the model and check the variable importance
fit <- lm(regression,data=review)
summary(fit)
anova(fit,test="Chisq")

# build the random forest model and double check the variable importance
set.seed(1234)
library(randomForest)
data_fac=review %>% mutate_if(is.character, as.factor)
rf.fit <- randomForest(regression,data=data_fac,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(rf.fit,color="blue",pch=20,cex=1.25,main="")
