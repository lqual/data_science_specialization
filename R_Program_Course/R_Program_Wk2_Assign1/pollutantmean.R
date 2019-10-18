pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names = TRUE) #File List
  dat <- data.frame() #create empty data frame
  for (i in id) { #id is range of files to use
    dat <- rbind(dat, read.csv(files_list[i])) #rbinds all files together
  }
  dat_subset <- dat[pollutant] #datasubset of pollutant name
  colMeans(dat_subset, na.rm = TRUE) #mean of pollutant column
}