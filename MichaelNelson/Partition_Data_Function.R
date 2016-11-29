#Partition Data Function
# This function splits datasets into two subsets - one for training and one for testing
# 11/23/2016

# Create a function that takes a dataset (x) and a proportion, between 0 & 1, that should be part of the training dataset (p). Testing dataset proportion = 1-p

datasplit <- function(x, p) {
  
  # Create new column for "train" and "test" labels
  x$Partrition <- 0
  x$Partrition <- factor(x$Partrition, ordered = TRUE, levels = c("train", "test"))
  
  
  # Number of observations that make up training data subst
  smp_size <- floor(p * nrow(x))
  
  # Set the seed to make your partition reproductible
  set.seed(2)
  trainindex <- sample(seq_len(nrow(x)), size = smp_size) # create a vector of random row numbers from dataset. N = smp_size.
  
  x[trainindex, "Partrition"] <- "train"   # "train" labels in x$Partition = all row numbers pulled into trainindex
  x[-trainindex, "Partrition"] <- "test"   # "test" labels in x$Partition = all row numbers pulled into trainindex
  
  
  
  return(x)
  
}

x <- datasplit(data, 0.9)

# Save to file, MyFunction.R
dump("datasplit", file = "C:/Users/michnelson/Desktop/Analytics Training/R Exercise/MyFunctions.R")



