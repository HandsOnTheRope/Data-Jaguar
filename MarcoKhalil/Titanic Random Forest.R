# install.packages('tree')
library(tree)
# install.packages('party')
library(party)
#install.packages('rattle')
library(rattle)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('rpart')
library(rpart)
#install.packages('caret')
library(caret)
#install.packages('lattice')
library(lattice)
#install.packages('RColorBrewer')
library(RColorBrewer)
#install.packages('randomForest')
library(randomForest)
#install.packages('e1071')
library(e1071)

titanicData <- read.csv(choose.files(), stringsAsFactors = FALSE)
summary(titanicData)
dim(titanicData)
str(titanicData)
titanicData$Survived <- factor(titanicData$Survived)
titanicData$Sex <- factor(titanicData$Sex)
titanicData$Embarked <- factor(titanicData$Embarked)
titanicData$Age <- as.numeric(titanicData$Age)

# Seed
set.seed(200)

# fill in misssing Age data
titanicData$Age[is.na(titanicData$Age)] <- mean(titanicData$Age,na.rm=T) 
summary(titanicData$Age)

# fill in missing Embarked data
which(titanicData$Embarked == '') # 62 830
titanicData$Embarked[c(62,830)] = "S"
titanicData$Embarked <- factor(titanicData$Embarked)

# fill in missing Fare data
# which(is.na(titanicData$Fare)) #### nothing is missing
# titanicData$Fare[1044] <- median(titanicData$Fare, na.rm=TRUE)

# Sample Indexes
indexes = sample(1:nrow(titanicData), size = .5*nrow(titanicData))

# Split Data
test = titanicData[indexes,]
dim(test) #445 x 12
train = titanicData[-indexes,]
dim (train) # 446 x 12

# run random forest model
forestModel <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + 
                              Parch + Fare + Embarked, data = train, importance = TRUE, ntree = 2000)

# which variables are important
varImpPlot(forestModel)

# Prediction Function
Prediction <- predict(forestModel,test)
Prediction
test$Survived

# try to compare Prediction & test$Survived to see how you did
# use below methods
# 1 - misclassification error 
# 2 - confusion matrix
# 3 - ROC curve

confusionMatrix(Prediction,test$Survived)