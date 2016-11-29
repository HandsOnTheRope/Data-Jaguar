#R-Training
#Random Forests Modelling 
#Titanic Dataset
#11/28/16

#Sections:
# A.Split Function
# 1.Loading the data
# 2.Cleaning the data 
# 3.Creating new variables
# 4.Splitting the data
# 5.Summary Statistics
# 6.Random Forests


############# SPLIT FUNCTION #############   

split_function <- function(df2,x,seed=TRUE) {
  #df is the raw data frame to augment
  #x is the percent of data in training df
  #seed is a boolean, if want it to use the seed
  
  #x% of the sample size
  smp_size <- floor(x * nrow(df2))
  
  #Set the seed to make your partition reproducible
  if (seed == TRUE) set.seed(123) else 
    print("Did not set seed")
  
  train_ind <- sample(seq_len(nrow(df2)), size = smp_size)
  
  #Create 2 data frames
  train2 <- df2[train_ind, ]
  test2 <- df2[-train_ind, ]
  
  #Create list of 2 df to return
  result <- list("Train"=train2,"Test"=test2)
  return(result)
  
}  

############# LOADING THE DATA ############# 

#Install extra packages if don't have them
#install.packages("broom") #for better output of regressions
#install.packages("ggplot2")
#install.packages("tree")
#install.packages("rpart")
#install.packages("party")
#install.packages("rattle")
#install.packages("caret")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
#install.packages("e1071")
#install.packages("pROC")
#install.packages("ROCR")
#install.packages("randomForest")
#install.packages(c("tidyr", "devtools"))
#install.packages("gridExtra")


#Load packages
library(party)
library(rpart)
library(rattle)
library(rpart)  
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(lattice)
library(e1071)
library(pROC)
library(ROCR)
library(ggplot2)
library(tree)
library(randomForest)
library(gridExtra)


#Set working directory
setwd("setyourfilepath") 

#Imports csv file
all.data <-
  read.csv(
    "C:/Users/SETYOURFILEPATH",
    header = TRUE
  )


############# CLEANING THE DATA ############# 

#Adressing missing values. 
#Variables missing values: Age, Cabin, and Embarked 

#Age: 92 NA's. Because this is a potentially important variable of interest, it doesn't make sense to drop these.
#Instead, will fill in with the mean of the age.

#Calculate the mean age
all.data$age_mean <- mean(all.data$Age,na.rm = TRUE)

#Create new varialbe with non missing values
all.data$age_clean <- ifelse(is.na(all.data$Age), all.data$age_mean, all.data$Age)

#Cabin: 91 NA's. Not as important of a variable. Class captures many of the same characteristics. No way to fill in.

#Embarked: 2 NA's. Could potentially just drop those 2. Will see if need to use it later. 


#Breaking up names into first, last
#Will do this in excel if have time ********

#Converting to correct variable type
#Numeric to factor
#Survived
all.data$Survived_factor <- factor(all.data$Survived, levels = c(1,0), labels = c("Survived", "Died"))

#Pclass
all.data$Pclass_factor <- factor(all.data$Pclass, levels = c(3,2,1), labels = c("Lower Class", "Middle Class", "Upper Class"))


############# CREATING NEW VARIABLES ############# 

#Family size variable is the sum of all siblings, spouses, parents, and children on board
all.data$family_sz <- all.data$SibSp + all.data$Parch

#Family flag to indicate wheather have a family
all.data$family_fl <- factor(ifelse(all.data$family_sz != 0, 1, 0),
                             levels = c(1,0),
                             labels = c("Has Family on Ship", "No Family on Ship"))      

#Child flag (if age < 18)
all.data$child <- factor(ifelse(all.data$age_clean < 18, 1, 0),
                         levels = c(1,0),
                         labels = c("Child", "Not Child"))

#Adult flag (if age 18-64)
all.data$adult <- factor(ifelse(all.data$age_clean > 17 & all.data$age_clean <65, 1, 0),
                         levels = c(1,0),
                         labels = c("Adult", "Not Adult"))

#Senior flag (if age > 64)
all.data$senior <- factor(ifelse(all.data$age_clean > 64, 1, 0),
                          levels = c(1,0),
                          labels = c("Senior", "Not Senior"))

############# SPLITTING THE DATA ############# 

#Create dataframe of variables to be used
used.data <- with(all.data, 
                data.frame(Pclass_factor, Sex, SibSp, Parch, Fare, age_clean, Survived_factor, family_sz))
#Can also add in Ticket and Embarked (has 2 missing values)
summary(used.data)



#Invoking the function to randomly split the data frame into train and test
y <- split_function(used.data, .5, TRUE)

#Access both data frames
#y$Train
#y$Test  

#Set the data frame to use
df <- y$Train

############# SUMMARY STATISTICS ############# 

#General dataset summary stats
str(df)
names(df)
summary(df)
class(df)
summary(df$Ticket)
# cor(all.data[c("FICO.Score","Interest.Rate.Clean", "Loan.Length.Clean")])

############# RANDOM FORESTS ############# 
#Resources: https://www.r-bloggers.com/predicting-wine-quality-using-random-forests/
  #https://rstudio-pubs-static.s3.amazonaws.com/22159_77fe3f8ed4cb4ab7b3647a8d5c628ca4.html
  #documentation: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
  #https://www.r-bloggers.com/part-3-random-forests-and-model-selection-considerations/
  #Overview: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#overview
# 
# #Create dataframe of used variables for train
# used_df <- with(df, 
#                 data.frame(Pclass_factor, Sex, SibSp, Parch, Fare, age_clean, Survived_factor, family_sz))
#   #Can also add in Ticket and Embarked (has 2 missing values)
# summary(used_df)
#   
# #Create dataframe of used variables for test
# test_df <- with(y$Test, 
#                 data.frame(Pclass_factor, Sex, SibSp, Parch, Fare, age_clean, Survived_factor, family_sz))

model <- randomForest(Survived_factor ~., data = df, na.action=na.omit)
model


#Plotting Model
imp<-importance(model)
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]
par(mfrow=c(1,2))
varImpPlot(model,main='Variable Importance Plot: Base Model', col="blue")
plot(model,main='Error vs No. of trees plot: Base Model', col="blue")


#Interpreting Results
#Confusion Matrix
test_df <- y$Test
testforest <- predict(model, newdata=test_df)
table(testforest, test_df$Survived_factor)

#ROC Curve
test.forest <- predict(model, type = "prob", newdata = test_df)
forestpred = prediction(test.forest[,2], test_df$Survived_factor)
forestperf = performance(forestpred, "tpr", "fpr")
plot(perf, main="ROC", colorize=T)

######################################################################


