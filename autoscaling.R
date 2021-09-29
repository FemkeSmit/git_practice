autoscaling <- function(dat){
  m <- colMeans(dat)
  s <- apply(dat,2,sd)
  dat2 <- sweep(dat,2,m,'-')
  dat2 <- sweep(dat2,2,s,'/')
  return(dat2)
}

