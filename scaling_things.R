#------------------#
#   Reading data   #
#------------------#
data <- read.delim("Data_tocheck.txt",header = FALSE)

#--------------------#
#   Options to set   #
#--------------------#
dat <- data

#--------------#
#   Function   #
#--------------#
cross_validate <- function(dat,cv_one = 5,cv_two = 10, scaling = "autoscaling",saveInner = TRUE){
# dat = data, cv_one = number of outer loops, cv_two = number of inner loops, 
# scaling = scaling functions from: 'autoscaling','mean_center','pareto'
  
  #Make sure caret is there
  if (!require('caret')){
    BiocManager::install('caret')
  }
  #Sourcing functions
  source("autoscaling.R")
  source("Mean_center.R")
  source("paretofunc.R")

  #outer test/train division
  results <- vector('list',cv_one)
  trainIndex <- caret::createDataPartition(1:nrow(data),times = cv_one,p = 0.8)
  #inner crossvalidation
  outerRes <- vector("list",length(trainIndex))
  for (i in 1:length(trainIndex)){
    trainSet <- data[trainIndex[[i]],]
    testSet <- data[-trainIndex[[i]],]
    trainCVIndex <- caret::createDataPartition(1:nrow(trainSet),times = cv_two,p = 0.8)
    innerRes <- vector("list",length(trainCVIndex))
    for (ii in 1:length(trainCVIndex)){
      trainSet2 <- trainSet[trainCVIndex[[ii]],]
      testSet2 <- trainSet[-trainCVIndex[[ii]],]
      #Scaling the test sets
      if (scaling == 'autoscaling'){
        scaled <- autoscaling(trainSet2)
        testSet2Scaled <- sweep(testSet2,2,scaled[[2]],'-')
        testSet2Scaled <- sweep(testSet2Scaled,2,scaled[[3]],'/')
        #scaling training set
        trainSet2Scaled <- scaled[[1]]
      }
      if (scaling == "mean_center"){
        scaled <- center_sweep(trainSet2)
        testSet2Scaled <- sweep(testSet2,2,scaled[[2]],'-')
        #scaling training set
        trainSet2Scaled <- scaled[[1]]

      }
      if (scaling == "pareto"){
        scaled <- paretoscaling(trainSet2)
        testSet2Scaled <- sweep(testSet2,2,scaled[[2]],'-')
        testSet2Scaled <- sweep(testSet2Scaled,2,scaled[[1]],'/')
        #scaling training set
        trainSet2Scaled <- scaled[[3]]

      }
      
      
      #Getting means variables in training and test sets, and comparing
      sampleMeanTrain <- colMeans(trainSet2Scaled)
      sampleMeanTest <- colMeans(testSet2Scaled)
      comparedMean <- t.test(sampleMeanTrain,sampleMeanTest)
      innerRes[[ii]] <- comparedMean
    }
    if (saveInner == TRUE){
      assign(paste0("innerRes_",i),innerRes,envir = .GlobalEnv)
    }
    #Scaling outer train and test sets
    if (scaling == 'autoscaling'){
      scaled <- autoscaling(trainSet)
      
      testSetScaled <- sweep(testSet,2,scaled[[2]],'-')
      testSetScaled <- sweep(testSetScaled,2,scaled[[3]],'/')
      #scaling training set
      trainSetScaled <- scaled[[1]]
    }
    if (scaling == "mean_center"){
      scaled <- center_sweep(trainSet)
      
      testSetScaled <- sweep(testSet,2,scaled[[2]],'-')
      trainSetScaled <- scaled[[1]]
    }
    if (scaling == "pareto"){
      scaled <- paretoscaling(trainSet)
      
      testSetScaled <- sweep(testSet,2,scaled[[2]],'-')
      testSetScaled <- sweep(testSetScaled,2,scaled[[1]],'/')
      trainSetScaled <- scaled[[3]]
    }

    
    #THIS IS WHERE COMPARAISON WITH INDEPENDENT TEST SET WOULD HAPPEN
    #Getting means variables in training and test sets, and comparing
    sampleMeanTrain <- colMeans(trainSetScaled)
    sampleMeanTest <- colMeans(testSetScaled)
    comparedMean <- t.test(sampleMeanTrain,sampleMeanTest)
    outerRes[[i]] <- comparedMean
  }
  return(outerRes)
}