# setting working directory
setwd("C:/Users/HanYong/Desktop")

# installing and loading readxl package 
library(readxl)
library(tidyverse)
library(stringr)
library(ggplot2)
library(party)

# 데이터 불러오기
leisure <- read_excel("DATA_2016_Leisure.xlsx", col_names = TRUE, na = "NA")
as.data.frame(leisure)
attach(leisure)

sum(leisure$Q18A1==1)
sum(leisure$Q18A1==2)
sum(leisure$Q18A1==3)
sum(leisure$Q18A1==4)
sum(leisure$Q18A1==5)
sum(leisure$Q18A1==6)
sum(leisure$Q18A1==7)



# 기초 통계 요약
str(leisure)
summary(leisure)

######### 상관도 출력 ##########
leisure_outlier <- leisure[c("Q9", "Q10", "Q13A1A1", "Q36", "Q24A1")]
as.data.frame(leisure_outlier)
#install.packages("corrplot")
library(corrplot)
M <- cor(leisure_outlier)
corrplot(M, method = "circle")


install.packages("DMwR")
library(DMwR)
library(psych)
pairs.panels(leisure_outlier, pch=21)
km.2 <- kmeans(leisure_outlier, centers= 2)
km.2$centers
outlier.scores <- lofactor(leisure_outlier, k=5)
plot(density(outlier.score))



outlier.score <- lofactor(leisure_outlier, k = 5) 
outlier.score <- na.omit(outlier.score)
plot(density(outlier.score), main = "outlier score of Leisure")
boxplot(outlier.scores, horizontal = TRUE)
outliers <- order(outlier.scores, decreasing = T) [1:6]
outliers
print(leisure_outlier[outliers,])
dim(leisure_outlier)
pch <- rep(".", 579)
pch[c(38,49,155,273)] <- "+"
col <- rep("black", 579)
col[c(38,49,155,273)] <- "red"
pairs(leisure_outlier, pch=pch, col=col, cex = 1.5)


# 주성분분석
biplot(prcomp(iris[ , 1:4]), cex = 0.8, xlabs = labels)

# 기초 통계 그래프
ggplot(leisure, aes(y=Q9))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('금액')+ggtitle('지난 1년간 여가활동에 사용한 월 평균 지출액')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(y=Q10))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('금액')+ggtitle('적절하다고 생각하는 여가비용(월 평균)')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(y=Q24A1))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('시간')+ggtitle('일 평균 스마트기기 이용 시간- 평일')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(y=Q13A1A1))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('시간')+ggtitle('지난 1년 간 하루 평균 여가시간 - 평일')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(y=Q43A3))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('시간')+ggtitle('주당 평균 근무 시간')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(y=Q36))+geom_boxplot(size=1.5,alpha=.3,col='blue')+ylab('나이')+ggtitle('응답자 연령')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(x=Q44A1))+geom_histogram(size=1.5,alpha=.3,col='blue', binwidth = 1)+ylab('금액')+ggtitle('지난 1년 간 세금 공제 전 월 평균 소득-본인')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(x=Q44A2))+geom_histogram(size=1.5,alpha=.3,col='blue', binwidth = 1)+ylab('금액')+ggtitle('지난 1년 간 세금 공제 전 월 평균 소득-가구 전체')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(x=Q35))+geom_histogram(size=1.5,alpha=.3,col='blue', binwidth = 1)+ylab('성별')+ggtitle('응답자 성별 - 남/녀')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(x=Q22))+geom_histogram(size=1.5,alpha=.3,col='blue', binwidth = 1)+ylab('여부')+ggtitle('여가활동을위한 동호회 참여 여부 - 예/아니오')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())

ggplot(leisure, aes(x=Q19))+geom_histogram(size=1.5,alpha=.3,col='blue', binwidth = 1)+ylab('여부')+ggtitle('생활권 내 공공문화여가시설 이용여부 - 예/아니오')+
  theme(axis.title.y=element_text(size=18,margin=margin(t=0,r=15,b=0,l=0)),axis.title.x=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        plot.title=element_text(size=20,hjust=0.5,face='bold',margin=margin(t=10,r=0,b=15,l=0)),
        panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank())


################## 기초 통계 그래프 끝 ###################


############ 주성분 분석 #################################
# "Q9" , "Q13A1A1", "Q24A1" , "Q36", "Q10"
leisure <- read_excel("DATA_2016_Leisure.xlsx", col_names = TRUE, na = "NA")
cor(leisure$Q9, leisure$Q10)

#### 주성분 분석 화살표 X 그래프 #####
data <- leisure[c("Q9" , "Q13A1A1", "Q24A1" , "Q36", "Q10")]
data.pca <- prcomp(data[c(1:5)], scale. = TRUE)
print(data.pca)
summary(data.pca)
#install.packages("devtools")
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(data.pca, obs.scale = 1, var.scale = 1, groups = , ellipse = TRUE, circle = TRUE)
print(g)

#### 주성분 분석 화살표 그래프 ######
hep.data.pca <- prcomp(data, scale = T)
summary(hep.data.pca)
screeplot(hep.data.pca, type="lines", pch=1, main="scree plot")
hep.data.pca$rotation[,1:3] 
hep.data.pca$x[,1:3]
biplot(hep.data.pca, main="Biplot")


############# 주성분 분석 끝 ################################

############### 대응 분석 ##################################

library(FactoMineR) 
library(factoextra) 
###### 대응분석 만족도 * 가족 소득 #######
x<-leisure %>% group_by(Q30, Q44A2) %>% summarise(n=n()) 
x
x[c(1:9),]
x[c(10:20),]
x[c(21:32),]
x[c(33:44),]
x[c(45:56),]
x[c(57:68),]
x[c(69:80),]

Q301 <- c(0,    6,  11,  13, 14,   16,   9,   8,   2,  0,  0,  1)
Q302 <- c(0,   26,  48,  68, 104,  79,  61,  17,   5,  2,  2,  3)
Q303 <- c(5,  103, 124, 206, 309, 252, 157,  74,  27, 24,  5,  6)
Q304 <- c(8,  221, 282, 472, 723, 613, 357, 146,  49, 33, 12, 22)
Q305 <- c(15, 310, 375, 554, 829, 861, 513, 222,  90, 38, 13, 10)
Q306 <- c(4,  144, 204, 256, 404, 364, 231,  94,  47, 25,  6, 18)
Q307 <- c(1, 18, 31, 25, 54, 46, 28, 22, 12, 7 ,1, 5)
its_for_ca <- cbind(Q301, Q302, Q303, Q304, Q305, Q306, Q307)

as.data.frame(its_for_ca)
fit = CA(its_for_ca)
summary(fit)
######## 그래프 타입 1 #########
fviz_ca_biplot(fit, mass = FALSE, contrib = "absolute",title = "CA_plot_Q44A2", map = "corprincipal", arrows = c(TRUE,TRUE)) + theme_minimal()
######## 그래프 타입 2 #########
fviz_ca_biplot(fit,title = "CA_plot_Q44A2") + theme_minimal()


####### 대응 분석 - 만족도 * 본인 소득 ########
setwd("C:/Users/HanYong/Desktop")
library(dplyr)
library(readxl)
leisure <- read_excel("DATA_2016_Leisure.xlsx", col_names = TRUE, na = "NA")
as.data.frame(leisure)
attach(leisure)
x<-leisure %>% group_by(Q30, Q44A1) %>% summarise(n=n()) 
x
x[c(1:8),]
x[c(9:17),]
x[c(18:28),]
x[c(29:40),]
x[c(41:52),]
x[c(53:63),]
x[c(64:72),]

Q301 <- c(20, 11, 17, 15, 10, 2, 3, 2,0, 0, 0, 0)
Q302 <- c(118, 41, 103, 64, 61, 13, 11,0, 3, 1,0,0)
Q303 <- c(384, 140, 273, 228, 178, 43, 27, 5, 11, 2, 1,0)
Q304 <- c(657, 257, 559, 521, 435, 117, 66, 9, 9, 3, 1, 4)
Q305 <- c(1391, 368, 668, 607, 497, 164, 95, 18, 9, 4, 5, 4)
Q306 <- c(644, 181, 317, 283, 236, 70, 37, 13, 7, 4, 0,5)
Q307 <- c(84, 24, 52, 24, 43, 10, 7, 4, 0,0,0,2)

its_for_ca <- cbind(Q301, Q302, Q303, Q304, Q305, Q306, Q307)

as.data.frame(its_for_ca)
fit = CA(its_for_ca)
summary(fit)
fviz_ca_biplot(fit, mass = FALSE, contrib = "absolute", title = "CA_plot_Q44A1",map = "corprincipal", arrows = c(TRUE,TRUE)) + theme_minimal()
fviz_ca_biplot(fit,title = "CA_plot_Q44A1") + theme_minimal()

#################### 대응 분석 끝 #####################


################### 여기 까지 하고 R 스튜디오 껐다가 다시 켜기 #############

#################### 의사 결정 나무 ###################

# setting working directory
setwd("C:/Users/HanYong/Desktop")

# installing and loading readxl package 
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(party)
library(caret)


# 데이터 불러오기
leisure <- read_excel("DATA_2016_Leisure.xlsx", col_names = TRUE, na = "NA")
leisure <- as.data.frame(leisure)
attach(leisure)


#sum(leisure["Q30"]==4)
leisure <- rename(leisure, leisure_purpose = Q3A1 , leisure_money = Q9, leisure_time = Q13A1A1 ,Smartphone_time = Q24A1, marriage = Q37,age = Q36, income = Q44A1, club = Q22, income_family = Q44A2, cost_proper = Q10, sex = Q35)
leisure$Q30 <- ifelse(leisure$Q30 > 4, "Yes", ifelse(leisure$Q30 < 4, "No", 4))
leisure$sex <- ifelse(leisure$sex ==1 , "Man","Woman" )
leisure$sex <- as.factor(leisure$sex)
leisure_satisfying <- subset(leisure, leisure$Q30 != "4")

############ 의사결정 나무 #########
weekday_leisure_culture_watching <- subset(leisure_satisfying, Q11A1 != "98")
weekday_leisure_culture_watching_selec <- weekday_leisure_culture_watching %>% select(leisure_money , leisure_time, Smartphone_time , Q30, age, income, club, income_family, sex, cost_proper)
weekday_leisure_culture_watching_selec$club <- as.factor(weekday_leisure_culture_watching_selec$club)
### 여기까지 대상 데이터 셋 설정 및 범주형 데이터 factor로 변환
set.seed(1234)
### 동일한 샘플 수집을 위한 seed 설정
ind <- sample(2, nrow(weekday_leisure_culture_watching_selec), replace = TRUE, prob = c(0.7, 0.3))
### train/ test 데이터 분할

trainData <- weekday_leisure_culture_watching_selec[ind == 1, ]
testData <- weekday_leisure_culture_watching_selec[ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
### 분류 기준 일반화/범주화
#View(trainData) 데이터 확인
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
### 나무 생성 및 그래프 설정

trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))

### confusionMatrix로 분류정확도확인


weekday_leisure_doing <- subset(leisure_satisfying, Q11B1 != "98")
weekday_leisure_doing_selec <- weekday_leisure_doing %>% select(leisure_money , leisure_time, Smartphone_time , Q30, age, income, club, income_family, sex, cost_proper)
weekday_leisure_doing_selec $club <- as.factor(weekday_leisure_doing_selec $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_doing_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_doing_selec [ind == 1, ]
testData <- weekday_leisure_doing_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
View(trainData)

Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))



weekday_leisure_sports_watching <- subset(leisure_satisfying,  Q11C1 != "98")
weekday_leisure_sports_watching_selec <- weekday_leisure_sports_watching %>% select(leisure_money , leisure_time, Smartphone_time , Q30, age, income, club, income_family, sex, cost_proper)
weekday_leisure_sports_watching_selec $club <- as.factor(weekday_leisure_sports_watching_selec $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_sports_watching_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_sports_watching_selec [ind == 1, ]
testData <- weekday_leisure_sports_watching_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)

Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))


weekday_leisure_sports_doing <- subset(leisure_satisfying,  Q11D1 != "98")
weekday_leisure_sports_doing_selec <- weekday_leisure_sports_doing %>% select(leisure_money , leisure_time, Smartphone_time , Q30, age, income, club, income_family, sex, cost_proper)
weekday_leisure_sports_doing_selec $club <- as.factor(weekday_leisure_sports_doing $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_sports_doing_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_sports_doing_selec [ind == 1, ]
testData <- weekday_leisure_sports_doing_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))



weekday_leisure_sight_seeing <- subset(leisure_satisfying,  Q11E1!= "98")
weekday_leisure_sight_seeing_selec <- weekday_leisure_sight_seeing %>% select(leisure_money , leisure_time, Smartphone_time ,  sex, cost_proper,Q30, age, income, club, income_family)
weekday_leisure_sight_seeing_selec$club <- as.factor(weekday_leisure_sight_seeing_selec $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_sight_seeing_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_sight_seeing_selec[ind == 1, ]
testData <- weekday_leisure_sight_seeing_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))



weekday_leisure_hobby_entertainment <- subset(leisure_satisfying, Q11F1 != "98")
weekday_leisure_hobby_entertainment_selec <-weekday_leisure_hobby_entertainment %>% select(leisure_money ,  sex, cost_proper,leisure_time, Smartphone_time , Q30, age, income, club, income_family)
weekday_leisure_hobby_entertainment_selec$club <- as.factor(weekday_leisure_hobby_entertainment_selec$club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_hobby_entertainment_selec), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_hobby_entertainment_selec[ind == 1, ]
testData <- weekday_leisure_hobby_entertainment_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))




weekday_leisure_rest <- subset(leisure_satisfying, Q11G1 != "98")
weekday_leisure_rest_selec <-weekday_leisure_rest %>% select(leisure_money , leisure_time, Smartphone_time , Q30, age, sex, cost_proper, income, club, income_family)
weekday_leisure_rest_selec $club <- as.factor(weekday_leisure_rest_selec $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_rest_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_rest_selec [ind == 1, ]
testData <- weekday_leisure_rest_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))



weekday_leisure_society_etc <- subset(leisure_satisfying, Q11H1 != "98")
weekday_leisure_society_etc_selec <-weekday_leisure_society_etc %>% select(leisure_money , leisure_time, Smartphone_time ,  sex, cost_proper, Q30, age, income, club, income_family)
weekday_leisure_society_etc_selec $club <- as.factor(weekday_leisure_society_etc $club)
set.seed(1234)
ind <- sample(2, nrow(weekday_leisure_society_etc_selec ), replace = TRUE, prob = c(0.7, 0.3))
trainData <- weekday_leisure_society_etc_selec [ind == 1, ]
testData <- weekday_leisure_society_etc_selec [ind == 2, ]
trainData$Q30 <- as.factor(trainData$Q30)
testData$Q30 <- as.factor(testData$Q30)
#View(trainData)
Q30_ctree <- ctree(Q30~., data = trainData)
plot(Q30_ctree)
plot(Q30_ctree, type = "simple")
trainPred <- predict(Q30_ctree, newdata = trainData)
trainPred
table(predict(Q30_ctree), trainData$Q30)
testPred <- predict(Q30_ctree, newdata = testData)
testPred
table(testPred, testData$Q30)
confusionMatrix(table(testPred, testData$Q30))


####################### 의사 결정 트리 끝 ###############################

####################### GLM 일반 선형 회귀 모델 시작 ####################

################ GLM ###################
# setting working directory
setwd("C:/Users/HanYong/Desktop")

library(readxl)
RAW <- read_excel("DATA_2016_Leisure.xlsx", col_names = TRUE, na = "NA")
attach(RAW)
RAW$Q30 <- ifelse(RAW$Q30 > 4, 1, ifelse(RAW$Q30 < 4, 0, 4))
RAW <- subset(RAW, RAW$Q30 != "4")
RAW$Q22 <- ifelse(RAW$Q22 ==1 , 1, 0)
RAW$Q19 <- ifelse(RAW$Q19 == 1, 1, 0)
RAW$Q35 <- ifelse(RAW$Q35 == 1, 1, 0)
as.data.frame(RAW)
RAW$Q35 <- as.factor(RAW$Q35)
RAW$Q22<- as.factor(RAW$Q22)
RAW$Q19<- as.factor(RAW$Q19)
RAW$Q44A1<- as.factor(RAW$Q44A1)
RAW$Q44A2<- as.factor(RAW$Q44A2)
sum(is.na(RAW$Q30))
#샘플링  > 일반화선형회귀분석
Sampling<-function(x,y,z){
  x[is.na(x)] <- 0
  a<-subset(x, x$Q36>(10*y-1) & x$Q36 < (10*z+10))
  select.column <- c("Q9" , "Q13A1A1", "Q24A1" , "Q30", "Q36", "Q44A1", "Q22", "Q44A2", "Q35", "Q19")
  subset(a, select = select.column)
}

SAMPLE_20<-Sampling(RAW,2,3) # 20대
SAMPLE_30<-Sampling(RAW,3,4) # 30대
SAMPLE_40<-Sampling(RAW,4,5) # 40대
SAMPLE_50<-Sampling(RAW,5,6) # 50대
SAMPLE_2060<-Sampling(RAW,2,7) # 20-60대
SAMPLE_60<-Sampling(RAW,6,7) # 60대
############# data에 매번 다른 SAMPLE_  을 입력해 줘야 합니다 ############

fit=glm(Q30 ~ Q9+Q13A1A1+Q24A1+Q36+Q44A1+Q22+Q44A2 +Q35+Q19, family = binomial, data=SAMPLE_2060)
exp(fit$coeff)

############### 과산포 검정 ##################
# 이항모형에서 잔차의 이탈도(residual deviance)와 잔차의 자유도를 비교해 보는 것이다. 
#이 비가 1이 훨씬 넘게 되면 과산포가 있다는 것을 시사해준다. 이 모형에서 계산해보면 다음과 같다.
# 이 비율이 1에 가까우므로 과산포는 없다는 것을 시사해준다. 
#과산포가 있는지 검정하는 방법은 먼저 glm()을 통한 모형적합을 두번 하는데 먼저 
#family=binomial로 한번 적합시키고(예를 들어 결과를 fit로 저장) family=quasibinomial로 
#한번 더 적합시켜(fit.od로 저장) 다음 검정을 시행하면 된다.
# pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual,lower=F)
# 이 검정 결과 p값이 0.05이하인 경우 과산포가 있다고 할수 있다. 대장암 데이타에 이검정을 적용해보면 다음과 같다.
##############################################

fit.od=glm(Q30 ~ Q9+Q13A1A1+Q24A1+Q36+Q44A1+Q22+Q44A2 +Q35+Q19, family = quasibinomial, data=SAMPLE_2060)
pchisq(summary(fit.od)$dispersion*fit$df.residual,
       fit$df.residual,lower=F)
result <- glm(Q30 ~ Q9+Q13A1A1+Q24A1+Q36+Q44A1+Q22+Q44A2 +Q35+Q19 ,family = binomial, data=SAMPLE_2060)
summary(result)

######### p가 0.05이상이므로 과산포 X

#GLM with Binomial

# backward elimination > stepwise logistic regression 
reduced.model = step(result)
summary(reduced.model)

#plot(reduced.model)
#case control study 이므로 odds ratio 
#Plot for Odds Ratios
#library(moonBook)
ORplot(reduced.model, main="Plot for Odds Ratios of Reduced Model") 
########## Odds Ratio Plot 을 95 % 신뢰구간으로 표현 ###########
ORplot(reduced.model,type=2,show.OR=FALSE,show.CI=TRUE,
       main="Plot for Odds Ratios; type=2, show.CI=TRUE") 

######### Odds Ratios 를 막대그래프 형태로 표현 #########
# Plot for Odds Ratios with Barplot
ORplot(reduced.model,type=3,main="Bar Plot for ORs with type=3")

# with 95 % confidence interval
ORplot(reduced.model,type=3,show.CI=TRUE,main="Bar plot for ORs with 95% CI")


######### 95퍼센트 신뢰 구간에서 각 회귀 계수 표현 #################
ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}

ORtable(reduced.model)


################# Cross Fold 기법으로 예측력 측정 ###########



########### 여기서도 SAMPLE_50 ######## 을 필요한 연령대별 데이터 샘플로 변경해 줘야 합니다 #####

############### 예측 타입 1 ##############
k=10
set.seed(21)
folds=sample(k,nrow(SAMPLE_30[c("Q9" , "Q13A1A1", "Q24A1" , "Q30", "Q36", "Q44A1", "Q22", "Q44A2", "Q35", "Q19"  )]),rep=TRUE)

#GLM
cv.folds_GLM=rep(NA,k)
for (j in 1:k) {
  fit.GLM_full<-glm(Q30~.,SAMPLE_20[folds!=j,],family=binomial)
  fit.GLM<-step(fit.GLM_full,method="both",trace=0)
  prob<-predict(fit.GLM,SAMPLE_30[folds==j,],type="response")
  yhat<-ifelse(prob>0.5,1,0)
  cv.folds_GLM[j]<-mean(yhat!=SAMPLE_30$Q30[folds==j])
}
(cv.err_GLM.g<-mean(cv.folds_GLM)) 

#ROC curve
library(ROCR)

p <- predict(fit.GLM, newdata=SAMPLE_20, type="response")
pr <- prediction(p, SAMPLE_20$Q30)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


############### 예측 타입 2 ##############
#install.packages("glmnet")
library(glmnet)
set.seed(21)
k=10
folds=sample(k,nrow(SAMPLE_30),rep=TRUE)
grid=10^seq(10,-2,length=100)

x=model.matrix(Q30~.,SAMPLE_30)[,-1]
cv.folds_Rid<-rep(NA,k)
for (j in 1:k) {
  train<-which(folds!=j)
  fit.Rid<-glmnet(x[train,],SAMPLE_30$Q30[train],alpha=0,lambda=grid,thresh=1e-12,family="binomial")
  set.seed(21)
  cv.out<-cv.glmnet(x[train,],SAMPLE_30$Q30[train],alpha=0,family="binomial")
  bestlam<-cv.out$lambda.min
  prob<-predict(fit.Rid,s=bestlam,newx=x[-train,],type="response")
  yhat<-ifelse(prob>0.5,1,0)
  cv.folds_Rid[j]<-mean(yhat!=SAMPLE_30$Q30[-train])
}
(cv.err_Rid<-mean(cv.folds_Rid))

p <- predict(fit.Rid, s=bestlam,newx=x, type="response")
pr <- prediction(p, SAMPLE_30$Q30)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






