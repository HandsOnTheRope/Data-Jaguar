
### MEMORY ##################################################################################################################

#Clear Memory
rm(list = ls()) 
gc()


### LOAD DATA ###############################################################################################################

#Set Directory 
setwd("C:/Users/sampahwa/Documents/R Scripts")

#Read in Data 
titanic <- read.csv("train.csv")
names(titanic)
attach(titanic)


### CLEAN DATA ##############################################################################################################

#Check for missing values
sapply(titanic,function(x) sum(is.na(x)))

#Replace missing Age values with the average for the data set
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)

#Change Survived to a factor
titanic$Survived <- as.factor(titanic$Survived)


### RANDOM FOREST ###########################################################################################################

library(randomForest)

#Random Forest prediction of Titanic data
amazon <- randomForest(Survived ~ Pclass + Embarked + Age + Sex + SibSp + Parch, data=titanic)
print(amazon)
importance(amazon)


### ANALYZE TREE ############################################################################################################

library(ROCR)

#Create ROC curve
black <- data.frame(predict(amazon, titanic, type = "prob")) 
monteverde <- prediction(black[2], titanic$Survived) 
daintree <- performance(monteverde, measure = "tpr", x.measure = "fpr") 
plot(daintree, main="Random Forest ROC") 

#Find the area under the curve
taiga <- performance(monteverde, measure = "auc") 
taiga <- taiga@y.values[[1]] 
print(taiga) 

