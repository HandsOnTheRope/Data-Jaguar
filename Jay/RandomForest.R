library(rpart)
library(rattle)
library(rpart.plot)
library(ROCR)
library(randomForest)

#Set seed to make random forest reproducible 
set.seed(343)

#read in splitdata function
source("C:/Users/jdessy/Documents/R//DecisionTree/splitdata.R")

#label data into train and test datasets
mydata = splitdata("C:/Users/jdessy/Documents/R/DecisionTree/train.csv", 0.5)

#Make categorical variables 
mydata$Survived.Cat <- factor(mydata$Survived,level=c("0","1"))
mydata$Pclass.Cat <- factor(mydata$Pclass,level=c("1","2","3"))

#Use a decision tree to predict what age will be 
Agefit <- rpart(mydata$Age ~ mydata$Pclass + mydata$Sex + mydata$SibSp + mydata$Parch + mydata$Fare + mydata$Embarked,
                  data=mydata[!is.na(mydata$Age),], 
                  method="anova")
mydata$Age[is.na(mydata$Age)] <- predict(Agefit, mydata[is.na(mydata$Age),])

#Fill in fare with the mean value
mydata$Fare[is.na(mydata$Fare)] <- mean(mydata$Fare,na.rm=T)

#split datasets in train and test datasets 
train <- subset(mydata[c("Sex","Age","Pclass.Cat","Survived.Cat")], mydata$label == "train")
test  <- subset(mydata[c("Sex","Age","Pclass.Cat","Survived.Cat")], mydata$label == "test")

#plant the forest and let it grow 
forest <- randomForest(Survived.Cat ~ Pclass.Cat + Sex + Age,
                    data=train, 
                    importance=TRUE, 
                    ntree=1000)

#See how model predicts survival using test dataset
Prediction <- predict(forest, test)

#plot an ROC curve for the forest
predroc <- data.frame(predict(forest, test, type = "prob")) 
pred <- prediction(predroc[2], test$Survived.Cat) 
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

#find the area under the curve 
auc <- performance(pred, measure = "auc") 
auc <- auc@y.values[[1]] 
print(auc) 


