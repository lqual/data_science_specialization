run_analysis <- function() {
    #load data file onto computer if it isn't already there
    if(!file.exists("project.zip")) {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url, "project.zip")
        unzip("project.zip")
    }
        
    #read data tables into R
    x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
    x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
    y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
        
    #read column names for x_test & x_train from features.txt
    #and change column names for test and train files
        #note: added "std ()" to the end of "activity" and "subject" names
        #to make removing column names without "mean" or "std" easier later 
    col_names <- read.table("UCI HAR Dataset/features.txt")
    col_names <- col_names$V2
    names(x_test) <- col_names
    names(y_test) <- "activity std()"
    names(subject_test) <- "subject std()"
    names(x_train) <- col_names
    names(y_train) <- "activity std()"
    names(subject_train) <- "subject std()"
        
    #combine all data.frames into one data.frame
    test <- cbind(subject_test, y_test, x_test)
    train <- cbind(subject_train, y_train, x_train)
    data <- rbind(test, train)
        
    #remove columns which don't contain mean or std in their names
    col_names <- names(data)
    place <- c(1:563)
    mean <- as.integer(grepl("mean", col_names))
    std <- as.integer(grepl("std", col_names))
    locate <- data.frame(place, mean, std)
    library(plyr)
    locate <- mutate(locate, keep = mean + std)
    locate <- subset(locate, locate$keep == 1)
    col_keep <- locate$place
    data <- data[, col_keep]
        
    #changing variable names to be more descriptive by adding "freq" and "time",
    #removing parentheses, and replacing "-" with "."
        #note: doing this before changing activity numbers
        #to shorten activity column name for code
    colnames(data)[1:2] <- c("subject", "activity")
    colnames(data) <- sub("^f", "freq", colnames(data))
    colnames(data) <- sub("^t", "time", colnames(data))
    colnames(data) <- gsub("[())]", "", colnames(data))
    colnames(data) <- gsub("-", ".", colnames(data))
        
    #changing activity numbers to activity names in "activity" column
    activ_num <- data$activity
    size <- length(activ_num)
    activ_name <- vector(mode = "character", length = size)
    for(i in 1:size) {
        new_name <- ifelse(activ_num[i] == 1, "walking",
                    ifelse(activ_num[i] == 2, "walk_upstairs",
                    ifelse(activ_num[i] == 3, "walk_downstairs",
                    ifelse(activ_num[i] == 4, "sitting",
                    ifelse(activ_num[i] == 5, "standing",
                    ifelse(activ_num[i] == 6, "laying", activ_num[i])))))) 
        activ_name[i] <- new_name
    }
    data$activity <- activ_name
    
    #creating a second data set with the average of each variable
    #for each activity and each subject
    library(dplyr)
    mean_col <- colnames(data)
    mean_col <- mean_col[3:81]
    ave_data <- data %>% group_by(subject, activity) %>% 
                    summarize_at(vars(mean_col), funs(mean))
    
    #final result is average of all variables for each subject and activity
    ave_data   
}














