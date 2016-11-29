# Using the Titanic data set, determine if a passenger will survive or not with Random Forest modeling
# 11/28/16

# Library for decision trees
    library(rpart)
    library(party)
    library(rattle)
    library(rpart.plot)
    library(caret)

#Library for ROC curves
    library(ROCR)

#Library for Random Forest
    library(randomForest)

# Reference user-generated functions to partrition data
    source("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/myfunctions.R")
    
    
#################### IMPORT AND CLEAN DATA ########################
    


# Use the train data,  load here make na blank
    data <- read.csv("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/train.csv", header = TRUE, na.strings=c(""))


# Check for missing values
    sapply(data,function(x) sum(is.na(x)))

# Age has too many missing values, but is most likely important, we will replace the NA with the average for the data set
    data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# Add replace missing values in Embarked with S
    which(is.na(data$Embarked))
    data$Embarked[c(62,830)] = "S"

# Make Pclass a factor
    data$Pclass <- factor(data$Pclass, ordered = TRUE, levels = c("3","2","1"))
    levels(data$Pclass) <- c("Third Class", "Second Class", "First Class")
    

# Partrition data. Train/All Data = .5
    data_clean <- datasplit(data, 0.5)
    train <- subset(data_clean[data_clean$Partrition == "train",])
    test <- subset(data_clean[data_clean$Partrition == "test",])
    
    
################### GENERATE RANDOM FOREST FROM TRAIN DATASET ###############################
    
    
# Random Forest
    fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, importance = TRUE, ntree = 1000)

# Review Forest Output
    print(fit)
    plot(fit)

# Analyze importance of explanatory variables
    importance(fit)
    varImpPlot(fit)

    
################## USE RANDOM FOREST TO PREDICT TEST DATASET OUTCOMES ######################

    
# Prediction of variable, "Survived", for test dataset
    pred_data <- data.frame(predict(fit, test, type = "class"))

# Tests for accuracy:
    # Simple misclassification error
      misclassificationError <- mean(pred_data != test$Survived)
      print(paste('Accuracy',1-misclassificationError))

      
    # Confusion Matrix
      confusionmat <- confusionMatrix(pred_data[[1]],test$Survived)
      confusionmat

      
    # ROC Curve with Area Under the Curve (AUC)
      predroc <- data.frame(predict(fit, test, type = "prob"))
      pr <- prediction(predroc[2], test$Survived)
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf)
      
      auc <- performance(pr, measure = "auc")
      auc <- auc@y.values[[1]]
      auc



