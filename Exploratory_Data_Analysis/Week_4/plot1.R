plot1 <- function() {
        #loads data file onto computer if it isn't already there
        if(!file.exists("plotData.zip")) {
                url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                download.file(url, "plotData.zip")
                unzip("plotData.zip")
        }
        
        #readdata into R
        data <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        #Create plot
        png("plot1.png", width = 480, height = 480)
        y <- as.data.frame(tapply(data$Emissions, data$year, sum))
        library(dplyr)
        y <- y %>% mutate(`tapply(data$Emissions, data$year, sum)`/1000)
        plot(c(1999, 2002, 2005, 2008), y[,2], type = "b", pch = 16, col = "red", 
             lwd = 2, xlab = "Year", ylab = "Emmissions (thousand tons)", 
             main = "Total Emmissions in US from PM2.5")
        dev.off()
}