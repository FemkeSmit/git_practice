#------------------#
#   Reading data   #
#------------------#
data <- read.delim("Data_tocheck.txt",header = FALSE)

#--------------------#
#   Options to set   #
#--------------------#
dat <- data
cv_one <- 5
cv_two <- 10
scaling <- "autoscaling"
#cross_validate <- function(dat,cv_one,scaling){
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
  #source("Range_Scaling.R")
  #outer test/train division
  results <- vector('list',cv_one)
  trainIndex <- caret::createDataPartition(1:nrow(data),times = cv_one,p = 0.8)
  #inner crossvalidation
  for (i in 1:length(trainIndex)){
    trainSet <- data[trainIndex[[i]],]
    testSet <- data[-trainIndex[[i]],]
    trainCVIndex <- caret::createDataPartition(1:nrow(trainSet),times = cv_two,p = 0.8)
    for (ii in 1:length(trainCVIndex)){
      trainSet2 <- trainSet[trainCVIndex[[ii]],]
      testSet2 <- trainSet[-trainCVIndex[[ii]],]
      #Scaling the test sets
      if (scaling == 'autoscaling'){
        scaled <- autoscaling(trainSet2)
        testSet2Scaled <- sweep(testSet2,2,scaled[[2]],'-')
        testSet2Scaled <- sweep(testSet2Scaled,2,scaled[[3]],'/')
        
        testSetScaled <- sweep(testSet,2,scaled[[2]],'-')
        testSetScaled <- sweep(testSetScaled,2,scaled[[3]],'/')
      }
      if (scaling == "mean_center"){
        scaled <- center_colmeans(trainSet2)
      }
      if (scaling == "pareto"){
        scaled <- paretoscaling(trainSet2)
      }
      #scaling training set
      trainSet2Scaled <- scaled[[1]]
      
      #THIS IS WHERE TRAINING AND INNER CROSS VALIDATION WOULD HAPPEN
    }
    #THIS IS WHERE COMPARAISON WITH INDEPENDENT TEST SET WOULD HAPPEN
  }
  #THIS IS WHERE OVERALL RESULTS WOULD BE ASSESSED
#}
