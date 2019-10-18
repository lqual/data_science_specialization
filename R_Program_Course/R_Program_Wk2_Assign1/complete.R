complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names = TRUE) #File List
  dat <- data.frame() #create empty data frame
  for (i in id) { #id is range of files to use
    dat <- rbind(dat, read.csv(files_list[i])) #rbinds all files together
  }
  dat <- na.omit(dat) #gets rid of the NA's
  result <- data.frame(matrix(nrow = length(id), ncol = 2)) #build a data.frame
  colnames(result) <- c("id", "nobs") #name the columns
  for (i in id) {
    nobs <- nrow(subset(dat, ID == i))  #number of rows with data
    place <- which(id %in% i)  #figuring out each i's place in the data.frame
    result[place, 1] <- i  #placing the id in the first column of the data.frame
    result[place, 2] <- nobs #placing the nobs value in the data.frame
  }
  result #spitting out the data.frame
}