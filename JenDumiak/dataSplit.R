# 1)	Specify the raw data frame (function input 1)
# 2)	Specify the percent of the data that is training data  (function input 2)
# 3)	Randomly label test and train observations 
# 4)	Return the labelled data frame (function output)

# This function will split the dataFrame into 2 groups, test and train and will then label each observation as test or train, will return a list that includes the original data frame with test/train in the last column and test & train datasets

dataSplit <- function(dataFrame, splitPercent, seed){
  # Split the data into two sets
  smp_size <- floor(splitPercent * nrow(dataFrame))
  
  # Set the seed to make your partition reproducible
  set.seed(seed)
  trainindex <- sample(seq_len(nrow(dataFrame)), size = smp_size)
  
  dataFrame[trainindex, 13]  <- "Train"
  dataFrame[-trainindex, 13] <- "Test"
  colnames(dataFrame)[13] <- "Label"
  train <- dataFrame[trainindex, ]
  test <- dataFrame[-trainindex, ]
  
  returnList <- list(dataFrame, train, test)
  return(returnList)
}