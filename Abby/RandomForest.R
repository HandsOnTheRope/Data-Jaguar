#import data
library(readr)
mydata <- read_csv("~/train.csv")

mydata$Sex <- factor(mydata$Sex)
mydata$Pclass <- factor(mydata$Pclass)

#make family size variable  
mydata$famsize <- mydata$SibSp + mydata$Parch + 1

#check for blanks or NAs in Age
which(is.na(mydata$Age))
which(mydata$Age == '')

#fix NAs in age
library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + famsize,
                  data=mydata[!is.na(mydata$Age),], 
                  method="anova")
mydata$Age[is.na(mydata$Age)] <- predict(Agefit, mydata[is.na(mydata$Age),])
summary(Agefit)
#check that NAs are gone
which(is.na(mydata$Age))

#check Embarked for blanks, NAs, convert to factor
which(mydata$Embarked == '')
which(is.na(mydata$Embarked))
mydata$Embarked[c(62,830)] = "S"
mydata$Embarked <- factor(mydata$Embarked)
summary(mydata$Embarked)

#check Fare for NAs or blanks
which(is.na(mydata$Fare))
which(mydata$Fare == '')

#split data into testing and training dataset 
dt = sort(sample(nrow(mydata), nrow(mydata)*.5))
train <- mydata[dt,]
test <- mydata[-dt,]

#install random forest
library(randomForest)

#set random seed so that results are reproducible
set.seed(415)

#make random forest
forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + famsize,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
plot(forest)

#look at what variables are important
varImpPlot(forest)

#predict test data based on forest
prediction <- data.frame(predict(forest, test, type = "class"))

#compare predictions to actual test data
library(caret)
confusionmat <- confusionMatrix(prediction[[1]], test$Survived)
print(confusionmat)

#make ROC curve
library(ROCR)
prediction2 <- data.frame(predict(forest, test, type = "prob"))
pred <- prediction(prediction2[2], test$Survived)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf)

#area under curve
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc 