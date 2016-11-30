splitdata <- function(dataset, percent) {

mydata <- read.csv(dataset, header = TRUE, na.strings=c(""))
  smp_size <- floor(percent * nrow(mydata))
  set.seed(225)
  train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
  train <- mydata[train_ind, ]
  train$label <- rep("train", nrow(train))
  test <- mydata[-train_ind, ]
  test$label <- rep("test",nrow(test))
  dataoutput = rbind(train,test)
  return(dataoutput)
}

