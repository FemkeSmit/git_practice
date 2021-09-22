autoscaling <- function(dat){
  m <- colMeans(dat)
  s <- apply(dat,2,sd)
  dat2 <- (dat - m)/s
  return(dat2)
}

