
data <- read.delim("Data_tocheck.txt",header = FALSE)
cross_validate <- function(dat,cv_one,scaling){
  ## dat = data, cv_one = number of outer loops, cv_two = number of inner loops, 
  ## scaling = scaling functions from: 'autoscaling','mean_center','pareto'
  
  #Make sure caret is there
  if (!require('caret')){
    BiocManager::install('caret')
  }
  #Sourcing functions
  source("autoscaling.R")
  source("Mean_center.R")
  source("paretofunc.R")
  source("Range_Scaling.R")
  #outer test/train division
  results <- vector('list',cv_one)
  trainIndex <- caret::createDataPartition(1:nrow(data),times = cv_one,p = 0.8,returnTrain = TRUE)
  #inner crossvalidation
  for (i in 1:length(trainIndex)){
    trainSet <- data[trainIndex[[i]],]
    testSet <- data[-trainIndex[[i]],]
    trainCVIndex <- caret::createDataPartition(1:nrow(trainSet),times = cv_two,p = 0.8,returnTrain = TRUE)
    
  }
}
