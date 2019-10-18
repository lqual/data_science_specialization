corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE) #File List
  
  vr <- c() #create vector
  for (i in 1:332) {
    if(nrow(na.omit(read.csv(files_list[i]))) >= threshold) { 
      vr[i] <- i
    } #find non-NA values to compare to threshold. Could have used complete.R function
  }
  id <- vr[!is.na(vr)] #remove NA's from vector
  
  cr <- c() #creates empty vector
  for (i in id) { #for datasets that meet the threshold
    dat <- read.csv(files_list[i]) #read them
    cr[i] <- cor(dat["sulfate"], dat["nitrate"], use = "complete.obs") 
  } #correlate and put into the vector
  final <- cr[!is.na(cr)] #remove any NA's
  final #show result
}