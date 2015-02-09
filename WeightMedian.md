polluntantmean <- function(directory, polluntant, id = 1:332) {
files_list <- list.files(directory, full.names = TRUE) #creates a list of files
dat <- data.frame() #creates an empty data frame
for (i in 1:332) {
# loops through the files, rbinding them together
dat <- rbind(dat, read.csv(files_list[i]))
}
dat_subset <- dat[which(dat[, "sulfate"] == sulfate), ] #subsets the rows that match the .polluntant. argument
mean(dat_subset[, "sulfate"], na.rm = TRUE) #identifies the median weight
# while stripping out the NAs
}
