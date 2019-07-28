##########################################################
#                                                        #
############### Indain liver patients  ###################
#                                                        #
##########################################################
#import library
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(gridExtra)
library(viridis)
library(purrr)
library(ggplot2)
library(corrplot)
library(ggthemes)
library(moonBook)
library(mvoutlier)
library(chemometrics)
library(rrcovHD)
library(DMwR)
library(rrcovHD)
library(class)
library(e1071)
library(klaR)
library(psych)

#setting working directory
setwd("C:/Users/HanYong/Desktop/naver_data_science_competition")

#load Data set
Indian_liver <- read.csv("indian_liver_patient_True_2.csv")

#copy data
data <- Indian_liver
data$Gender<-as.factor(ifelse(data$Gender=="Male",1,0))
data$Dataset <- ifelse(data$Dataset==2, 0, data$Dataset)
as.data.frame(data)

#data preprocessing
data <- rename(data, ALT = Alamine_Aminotransferase)
data <- rename(data, AST = Aspartate_Aminotransferase)
data <- rename(data, ALP = Alkaline_Phosphotase)
View(data)

#data exploration
head(data)
dim(data)
str(data)
summary(data)
table(data)
View(data)

#이상치 탐색
data2<-data[,-11]
data3<-data2[,-2]
pairs.panels(data3,pch=21)
km.2<-kmeans(data3,centers=2)
km.2$centers
outlier.scores<-lofactor(data3,k=5)
plot(density(outlier.scores))
boxplot(outlier.scores,horizontal=TRUE)
outliers<-order(outlier.scores,decreasing=T)[1:4]
print(data3[outliers,])
dim(data3)
pch<-rep(".",579)
pch[c(200,136,118,543)]<-"+"
col<-rep("black",579)
col[c(200,136,118,543)]<-"red"
pairs(data3,pch=pch,col=col)
data_final<-data[-c(200,136,118,543),]
data <- data_final

disease_data <- data %>% filter(Dataset == 1) 
NO_disease_data <- data %>% filter(Dataset == 0)
pairs.panels(disease_data, pch = 21)
pairs.panels(NO_disease_data, pch = 21)
######## finish data processing
#Graphing 1
v1 <- data %>% group_by(Gender) %>% filter(Dataset == 1) %>% summarize(number = n())
v2 <- data %>% group_by(Gender) %>% filter(Dataset == 0) %>% summarize(number = n())

qty <- data.frame(disease = v1, No_disease = v2)
qty

v1 <- c(90, 49)
v2 <- c(320, 116)

qty <- data.frame(Female = v1, male = v2)
qty

barplot(as.matrix(qty), main = "성별에 따른 질병 여부", ylim = c(0,600), col = c("darkgreen", "cornsilk"), space = 0.1, cex.axis = 0.8, las = 1, names.arg = c("Female", "Male"), cex = 0.8)
legend(1.8, 600, c("disease", "No_disease"), cex = 0.9, fill = c("darkgreen", "cornsilk"))
select <- dplyr::select
data_disease <- data %>% filter(Dataset == 1) %>% select(-Dataset)
data_No_disease <- data %>% filter(Dataset == 0) %>% select(-Dataset)
head(data_disease)
head(data_No_disease)
#Graphing 2

ggplot(data, aes(x= Dataset==1, y=Age))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('나이')+ggtitle('나이에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=Total_Bilirubin))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('Total Bilirubin')+ggtitle('Total Bilirubin에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=Direct_Bilirubin))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('Direct Bilirubin')+ggtitle('Direct Bilirubin에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=ALP))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('ALP')+ggtitle('ALP에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=ALT))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('ALT')+ggtitle('ALT에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=AST))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('AST')+ggtitle('AST에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=Total_Protiens))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('Total Protiens')+ggtitle('Total Protiens에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=Albumin))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('Albumin')+ggtitle('Albumin에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(data, aes(x= Dataset==1, y=Albumin_and_Globulin_Ratio))+geom_boxplot(size=1.5,alpha=.3,col='blue')+xlab('질병 여부')+ylab('Albumin_and_Globulin_Ratio')+ggtitle('Albumin_and_Globulin_Ratio에 따른 질병 여부')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

#Data processing for NA and make copy
data1 <- na.omit(data)

#과산포(Overdispersion) 검정
#case control study 이므로 odds ratio 
fit=glm(Dataset ~ Age+Total_Bilirubin+Direct_Bilirubin+ALP+ALT+AST+Total_Protiens+Albumin+Albumin_and_Globulin_Ratio  ,family = binomial, data=data1)
exp(fit$coeff)

fit.od=glm(Dataset ~ Age+Total_Bilirubin+Direct_Bilirubin+ALP+ALT+AST+Total_Protiens+Albumin+Albumin_and_Globulin_Ratio  ,family = quasibinomial, data=data1)
pchisq(summary(fit.od)$dispersion*fit$df.residual,
       fit$df.residual,lower=F)
result <- glm(Dataset ~ Age+Total_Bilirubin+Direct_Bilirubin+ALP+ALT+AST+Total_Protiens+Albumin+Albumin_and_Globulin_Ratio  ,family = binomial, data=data1)
summary(result)
# p가 0.05이상이므로 과산포 X

#GLM with Binomial


# backward elimination > stepwise logistic regression 
reduced.model = step(result)
summary(reduced.model)

#Plot for Odds Ratios
ORplot(reduced.model, main="Plot for Odds Ratios of Reduced Model") 
ORplot(reduced.model,type=2,show.OR=FALSE,show.CI=TRUE,
       main="Plot for Odds Ratios; type=2, show.CI=TRUE") 

#Plot for Odds Ratios with Barplot
ORplot(reduced.model,type=3,main="Bar Plot for ORs with type=3")
#with 95 % confidence interval
ORplot(reduced.model,type=3,show.CI=TRUE,main="Bar plot for ORs with 95% CI")


ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}

ORtable(reduced.model)

k=10
set.seed(21)
folds=sample(k,nrow(data),rep=TRUE)

#GLM
cv.folds_GLM=rep(NA,k)
for (j in 1:k) {
  fit.GLM_full<-glm(Dataset~.,data[folds!=j,],family=binomial)
  fit.GLM<-step(fit.GLM_full,method="both",trace=0)
  prob<-predict(fit.GLM,data[folds==j,],type="response")
  yhat<-ifelse(prob>0.5,1,0)
  cv.folds_GLM[j]<-mean(yhat!=data$Dataset[folds==j])
}
(cv.err_GLM.g<-mean(cv.folds_GLM)) 

#ROC curve
library(ROCR)

p <- predict(fit.GLM, newdata=data, type="response")
pr <- prediction(p, data$Dataset)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

install.packages("glmnet")
library(glmnet)
set.seed(1)
k=10
folds=sample(k,nrow(data_final),rep=TRUE)
grid=10^seq(10,-2,length=100)

x=model.matrix(Dataset~.,data)[,-1]
cv.folds_Rid<-rep(NA,k)
for (j in 1:k) {
  train<-which(folds!=j)
  fit.Rid<-glmnet(x[train,],data$Dataset[train],alpha=0,lambda=grid,thresh=1e-12,family="binomial")
  set.seed(21)
  cv.out<-cv.glmnet(x[train,],data$Dataset[train],alpha=0,family="binomial")
  bestlam<-cv.out$lambda.min
  prob<-predict(fit.Rid,s=bestlam,newx=x[-train,],type="response")
  yhat<-ifelse(prob>0.5,1,0)
  cv.folds_Rid[j]<-mean(yhat!=data$Dataset[-train])
}
(cv.err_Rid<-mean(cv.folds_Rid))



p <- predict(fit.Rid, s=bestlam,newx=x, type="response")
pr <- prediction(p, data_final$Dataset)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
