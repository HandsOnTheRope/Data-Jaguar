#PACKAGES


library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(ROCR)
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
library(rattle)
library(randomForest)
library(ROCR)
library(rpart.plot)
library(caret)
library(ggplot2)

# Read in Callie Test
source("C:/Users/mmigdalovich/Documents/Titanic/CallieTest.R")

#Read in data
titanicdata <- read.csv("C:/Users/mmigdalovich/Documents/train.csv")
summary(titanicdata)

titanicdata$Age[is.na(titanicdata$Age)] <- mean(titanicdata$Age,na.rm=T) 

#split the data using the function
y <- CallieTest(titanicdata,.50, TRUE) 
train<- data.frame(y[1])
test<-data.frame(y[2])

# Create levels
titanicdata$Pclass <- factor(titanicdata$Pclass, levels=c("3","2","1"), ordered=TRUE)
titanicdata$Age[is.na(titanicdata$Age)] <- mean(titanicdata$Age,na.rm=T) # filling in missing ages

#My Forest
BlackForest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age, data=train, importance=TRUE, ntree=2000)


# view results 
print(BlackForest) 

# importance of each predictor
imp <- importance(BlackForest) 

#Subset of the Data
newdata <-subset(test, select=c(3, 5, 6)) 
My_PredictionRF<- data.frame(predict(BlackForest, newdata, type="class"))


#misclassification rate
confusionmat <- confusionMatrix(My_PredictionRF[[1]], test$Survived)  #confusion matrix
confusionmat

#ROC Curve
My_PredictionRF2<- data.frame(predict(BlackForest, newdata, type="prob"))
pr <- prediction(My_PredictionRF2[2], test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



#Plotting
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]
par(mfrow=c(1,2))


varImpPlot(BlackForest,main='Variable Importance Plot: Base Model')
plot(BlackForest,main='Error vs No. of trees plot: Base Model')








