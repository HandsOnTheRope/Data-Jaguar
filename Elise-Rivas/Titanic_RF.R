library(randomForest)
library(caret)

# load data
setwd("C:/Users/elrivas/Documents/Trainings/R Training/Titanic")
total_data <- read.csv("C:/Users/elrivas/Documents/Trainings/R Training/Titanic/train.csv")

# Set missing age values to mean
total_data$Age[is.na(total_data$Age)] <- mean(total_data$Age, na.rm=TRUE)
total_data$fam_size <- (total_data$SibSp+total_data$Parch)+1

# Factor variables
total_data$Survived <- as.factor(total_data$Survived)
total_data$Embarked <- as.factor(total_data$Embarked)
total_data$Sex <- as.factor(total_data$Sex)
total_data$Pclass <- factor(total_data$Pclass, levels=c(3,2,1), order = TRUE)

# Data splitting function
split_function <- function(raw, prop.split){
  # randomize
  raw <- raw[sample(1:nrow(raw)),]
  # new variable, make default "train" for now
  raw$test_or_train <- "train"
  raw$test_or_train[sample(1:nrow(raw), size=((prop.split)*nrow(raw)))] <- "test"
  return(raw)
}

# Run function
new_data <- split_function(total_data, .5)
# Separate into two
train <- subset(new_data, test_or_train=="train")
test <- subset(new_data, test_or_train=="test")

# Run random forest
set.seed(500)
rf <- randomForest(Survived ~ Pclass + Sex + Age + fam_size + Fare + Embarked, 
                   data=train, importance=TRUE, ntree=1250)
# Sex and Pclass are most important
varImpPlot(rf)
pred <- predict(rf, test)

# Find accuracy rate, usually in 81-84% range
accuracy <- confusionMatrix(data=pred, reference = test$Survived)$overall[1]
print(accuracy)