# Library for random forest
library(randomForest)
# Library for decision trees
library(rpart)
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# Library for k-fold cross validation function and confusionMatrix
library(caret)
# Library for ROC curves
library(ROCR)
# Splitting data function
source("C:/Users/jdumiak/Documents/Titanic/dataSplit.R")

# Use the train data,  load here make na blank
titanic <- read.csv("C:/Users/jdumiak/Documents/Titanic/train.csv", header = TRUE, na.strings=c(""))
# 1 is survived, 0 is died 
# For class, 1 = 1st class, 2 = 2nd class, 3 = third class
# sibsp=Number of Siblings/Spouses Aboard
# parch=Number of Parents/Children Aboard
# ticket=Ticket Number
# fare=Passenger Fare
# cabin=Cabin
# embarked=Port of Embarkation

# Check for missing values
sapply(titanic,function(x) sum(is.na(x)))
# Cabin has way too many missing values, drop this variable immeadiately
# Also Pasenger ID since it is an index, name because it is too specific and ticket
# Check for unique values
sapply(training.data.raw, function(x) length(unique(x)))

# Age has too many missing values, but is most likely important, we will replace the NA with whatever our decision tree predicts
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=titanic[!is.na(titanic$Age),], 
                method="anova")
titanic$Age[is.na(titanic$Age)] <- predict(Agefit, titanic[is.na(titanic$Age),])

# Embark has two blanks, a majority have 'S' so we will just replace those NA with that value
titanic$Embarked[c(which(is.na(titanic$Embarked)))] <- "S"

# Fare has no NA value so good here

# Factor data
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c("3","2","1"))
# Rename, 1st class is better than third class
levels(titanic$Pclass) <- c("Third Class", "Second Class", "First Class")
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

# Function to split data, dataSplit
outputList <- dataSplit(titanic, 0.5, 123)
titanic <- data.frame(outputList[1])
train <- data.frame(outputList[2])
test <- data.frame(outputList[3])

# Create the random forest with 2000 trees from the train set, also monitor what variables are important 
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + SibSp + Parch +                      Embarked,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

# See what variables are significant
print(fit) # view results 
plot(fit) # see where the ntree flattens the error out 
# The accuracy one tests to see how worse the model performs without each variable, so a high decrease in accuracy would be expected for very predictive variables
# The Gini one measures how pure the nodes are at the end of the tree and tests to see the result if each variable is taken out--high score means the variable was important
importance(fit) # importance of each predictor
varImpPlot(fit)
# Variables most important are Sex, Fare, Pclass, and Age rest are insigificant to the model

# Predict with the test data
pred <- data.frame(predict(fit, test, type = "class"))

# Look at the misclassification error to see how well model is doing
misclassificationError <- mean(pred != test$Survived)
print(paste('Accuracy',1-misclassificationError))

# Plots to visualize performance & evaluation
# Confusion matrix
confusionmat <- confusionMatrix(pred[[1]], test$Survived)

# ROC Curve + AUC
# Predictions are your continuous predictions of the classification, the labels are the binary truth for each variable
predroc <- data.frame(predict(fit, test, type = "prob"))
pr <- prediction(predroc[2], test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Second way to do random forest with party package
set.seed(123)
fit2 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +                       Embarked,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))

pred2 <- predict(fit2, test, OOB=TRUE, type = "response")

# Look at the misclassification error to see how well model is doing
misclassificationError <- mean(pred2 != test$Survived)
print(paste('Accuracy',1-misclassificationError))
# Doesn't improve accuracy so stopping here
