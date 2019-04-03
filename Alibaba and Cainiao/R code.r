setwd("C:/STONY/Practice/R (No.7)/data_1")
library(dplyr)
library(plyr)
library(randomForest)

##########################################
# Q1&Q2 reputation and customer behavior #
##########################################

### read the data ###
seller1 <- read.csv("C:/STONY/Practice/R (No.7)/data_8/msom_seller_data.csv",header=FALSE)
colnames(seller1) <- c("day","merchant_id","subcategory_id","pc_pv","pc_uv",
                    "app_pv","app_uv","avg_logistic_review_score","avg_order_quality_score",
                    "avg_service_quality_score","if_cainiao")
odr1 <- read.csv("msom_order_data_1.csv",header=FALSE)
colnames(odr1) <- c("day","order_id","item_det_info","pay_timestamp","buyer_id",
                    "promise_speed","if_cainiao","merchant_id","Logistics_review_score")

seller1$cainiao <- ifelse(seller1$if_cainiao==0,"No","Yes")
seller1$cainiao <- factor(seller1$cainiao,levels=c("No","Yes"))

######## A. how rep system affects the purchase########
### subset the data ###
odr1_num <- ddply(odr1,.(merchant_id,day),summarize,odr_number=length(order_id))
#dim(odr1_num)
seller_na <- na.omit(seller)
seller1_rep <- ddply(seller_na,.(merchant_id,day),summarize,
                     log_score=sum(avg_logistic_review_score)/length(avg_logistic_review_score),
                     qual_score=sum(avg_order_quality_score)/length(avg_order_quality_score),
                     serv_score=sum(avg_service_quality_score)/length(avg_service_quality_score)
                     )
### merge the data into one table ###
cutomer_behiv <- merge(odr1_num, seller1_rep, all=TRUE, sort=TRUE)
cutomer_behiv <- na.omit(cutomer_behiv)

### calculate the odr number of every merchant in one month###
customer_behiv <- ddply(cutomer_behiv,.(merchant_id),summarize,
                     odr_num=sum(odr_number),
                     log_score=sum(log_score)/length(log_score),
                     qual_score=sum(qual_score)/length(qual_score),
                     serv_score=sum(serv_score)/length(serv_score)
                     )

### the relationship between rep_score and purchase ###
model_odr <- {odr_num~log_score+qual_score+serv_score}
fit <- lm(model_odr,data=customer_behiv)
summary(fit)
anova(fit,test="Chisq")
step(fit,direction="both") 

### important preditors ###
set.seed(1234)
rf_odr <- randomForest(model_odr,data=customer_behiv,mtry=3,ntree=1000,importance=TRUE)
varImpPlot(rf_odr,color="blue",pch=20,cex=1.25,main="")



######## B. some indexes that affect the purchase########
# build the linear model
seller_na <- na.omit(seller)
# PC_PV
model_pcpv <- {pc_pv~avg_logistic_review_score+avg_order_quality_score+avg_service_quality_score+pc_uv+
    app_uv+app_pv+if_cainiao}
fit <- lm(model_pcpv,data=seller_na)
summary(fit)
anova(fit,test="Chisq")
step(fit,direction="both") 
# PC_uV
model_pcuv <- {pc_uv~avg_logistic_review_score+avg_order_quality_score+avg_service_quality_score+pc_pv+
    app_uv+app_pv+if_cainiao}
fit <- lm(model_pcuv,data=seller_na)
anova(fit,test="Chisq")
step(fit,direction="both") 
# APP_PV
model_apppv <- {app_pv~avg_logistic_review_score+avg_order_quality_score+avg_service_quality_score+pc_pv+
    app_uv+pc_uv+if_cainiao}
fit <- lm(model_apppv ,data=seller_na)
anova(fit,test="Chisq")
step(fit,direction="both") 
# APP_UV
model_appuv <- {app_uv~avg_logistic_review_score+avg_order_quality_score+avg_service_quality_score+pc_pv+
    app_pv+pc_uv+if_cainiao}
fit <- lm(model_appuv ,data=seller_na)
anova(fit,test="Chisq")
step(fit,direction="both") 



############################################
# Q3&4 analyze Cainiao services' influence #
############################################

### the informatio about using cainiao or not
odr1_sum <- ddply(odr1,.(merchant_id,if_cainiao),summarize,odr_sum=length(order_id))
seller1_cai <- ddply(seller_na,.(merchant_id,if_cainiao),summarize,
                    log_score=sum(avg_logistic_review_score)/length(avg_logistic_review_score),
                    qual_score=sum(avg_order_quality_score)/length(avg_order_quality_score),
                    serv_score=sum(avg_service_quality_score)/length(avg_service_quality_score)
                    )
cai_evaluate<- merge(odr1_sum, seller1_cai,by=c("merchant_id","if_cainiao"))
head(cai_evaluate)

cai_scr_odr <- ddply(cai_evaluate,.(if_cainiao),summarize,odr=sum(odr_sum),
                     log_score=sum(log_score)/length(log_score),
                     qual_score=sum(qual_score)/length(qual_score),
                     serv_score=sum(serv_score)/length(serv_score)
)
head(cai_scr_odr)


### the percentage of using cainiao or not ###
odr1_cai <- ddply(odr1,.(if_cainiao),summarize,odr_number=length(order_id))

cai_percent <- ddply(odr1_cai, 
           .(), 
           .fun=function(x){
             transform(x, percentage=with(x,ave(odr_number,if_cainiao)/sum(odr_number)))
           })
cai_percent

############################################
# Q6 analyze Cainiao services' influence #
############################################



