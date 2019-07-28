############################################################
#
#   Classification: Decision Tree 
#
##########################################################

install.packages("tidyverse")
library(tidyverse)

#
# Titanic Case
#

# read the titanic data set
setwd("C:/Users/HanYong/Desktop/R")
titanic <- read.csv("titanic_data.csv")
str(titanic)
summary(titanic)

titanic$Class <- factor(titanic$Class, labels=c("Crew", "First","Second","Third"))
titanic$Age <- factor(titanic$Age, labels=c("Child","Adult"))
titanic$Sex <- factor(titanic$Sex, labels=c("Female","Male"))
titanic$Survived <- factor(titanic$Survived, labels=c("No", "Yes"))

str(titanic)
summary(titanic)

# descriptive statistics
attach(titanic)
table(Survived, Class)
table(Survived, Age)
table(Survived, Sex)

barplot (table(Survived, Class), beside=TRUE)
barplot(table(Survived, Age), beside=TRUE)
barplot(table(Survived, Sex), beside=TRUE)

install.packages("gmodels")
library(gmodels)

CrossTable(Survived, Class)
chisq.test(Survived, Class)

CrossTable(Survived, Age)
chisq.test(Survived, Age)

CrossTable(Survived, Sex)
Achisq.test(Survived, Sex)

detach(titanic)

#  Classification with Decision Trees
#  Divide data into training and testing data sets

set.seed(1234)
train <- sample(nrow(titanic), 0.7*nrow(titanic))
titanic.train <- titanic[train, ]
titanic.test <- titanic[-train, ]

# Building Decision Tree Models with the rpart package

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

set.seed(1234)
dtree1 <- rpart(Survived~., data=titanic.train, method="class")
summary(dtree1)
plotcp(dtree1)

prp(dtree1, type=2, extra=104, fallen.leaves = TRUE)

#  validate with prediction error rates

# prediction for training data
dtree1.train.pred <- predict(dtree1, titanic.train, type="class")
table(titanic.train$Survived, dtree1.train.pred, dnn=c("Actual","Predicted"))

install.packages("caret")
library(caret)
install.packages("lattice")
install.packages('e1071', dependencies=TRUE)
confusionMatrix(dtree1.train.pred, titanic.train$Survived)

# prediction for testing data
dtree1.test.pred <- predict(dtree1, titanic.test, type="class")
table(titanic.test$Survived, dtree1.test.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(dtree1.test.pred, titanic.test$Survived)

# Building Decision Tree models with the party package

install.packages("party")
library(party)

dtree2 <- ctree(Survived~., data=titanic.train)
dtree2
plot(dtree2)

dtree2.test.pred <- predict(dtree2, titanic.test, type="response")
table(titanic.test$Survived, dtree2.test.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(dtree2.test.pred, titanic.test$Survived)

# Classifiction with Random Forests

install.packages("randomForest")
library(randomForest)

set.seed(1234)
rforest <- randomForest(Survived~., data=titanic.train, importance=TRUE)
rforest
importance(rforest, type=2)

rforest.test.pred <- predict(rforest, titanic.test)
table(titanic.test$Survived, rforest.test.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(rforest.test.pred, titanic.test$Survived)
