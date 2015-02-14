complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  mydatafiles <- c()
  mydatafiles <- c(mydatafiles, paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep=""))
  # read all files above
  mydata <- lapply(mydatafiles, read.csv)
  mydata <- do.call(rbind, mydata)
  #get completely observed cases
  mydata <- mydata[complete.cases(mydata), ]
  # create character vector to hold results
  data_vector <- c()
  # break data into subsets based on monitor ID
  # assign results of each subset to data_vector
  for (j in 1:length(id)) {
    data <- subset(mydata, mydata[,4]==id[j])
    data_vector <- c(data_vector, id[j], nrow(data))
  } 
  # convert vector to matrix
  data_matrix <- matrix(data_vector, nrow=length(id), ncol=2, byrow=TRUE)
  #convert matrix to data frame
  finaldata <- data.frame(data_matrix)
  # rename columns
  colnames(finaldata) <- c("id", "nobs")
  
  return(finaldata)
}