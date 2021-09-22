#Autoscaling

testData <- matrix(1:12,ncol = 3) #Example data
autoscaling <- function(dat){
  m <- colMeans(dat)
  s <- apply(dat,2,sd)
  dat2 <- (dat - m)/s
  return(dat2)
}
test <- autoscaling(testData)
