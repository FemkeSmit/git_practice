

#function for doing paretoscaling
#input should be the file you want to pareto scale centre, with samples across rows
paretoscaling <- function(file){
  file <- as.matrix(file) #making sure the file is useable as a matrix
  means_columns <- colMeans(file)  #calculating the mean for each column
  st_dev_columns <- apply(file, 2, sd)   #calculating the sd for each column
  sq_rt_st_dev_columns <- sqrt(st_dev_columns) #calculating the sqare root of the sd of each column
  scaled_file <- sweep(file, 2, means_columns, '-') #subtracting the mean of each column from all the values in that column 
  scaled_file <- sweep(scaled_file, 2, sq_rt_st_dev_columns, '/') #dividing the mean-centered values by the sqrt
  return(scaled_file)
}