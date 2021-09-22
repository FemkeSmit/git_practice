# small dataset
set.seed(212)
Data = matrix(rnorm(15), 5, 3)

Data

# center with 'colMeans()'
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

# apply it
center_colmeans(Data)