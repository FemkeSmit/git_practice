# # small dataset
# set.seed(212)
# testarooni = matrix(rnorm(15), 5, 3)



# center with 'colMeans()'
center_sweep <- function(x, row.w = rep(1, nrow(x))/nrow(x)) {
  get_average <- function(v) sum(v * row.w/sum(row.w))
  average <- apply(x, 2, get_average)
  sweep(x, 2, average)
}

# apply it
#center_sweep(testarooni)