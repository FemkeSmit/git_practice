autoscaling <- function(dat){
  m <- colMeans(dat)
  s <- apply(dat,2,sd)
  res <- vector('list',3)
  
  dat2 <- sweep(dat,2,m,'-')
  dat2 <- sweep(dat2,2,s,'/')
  res[[1]] <- dat2
  res[[2]] <- m
  res[[3]] <- s
  names(res) <- c('matrix','mean','sd')
  return(res)
}

