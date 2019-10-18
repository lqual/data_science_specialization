plot2 <- function() {
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
        png("plot2.png", width = 480, height = 480)
        library(dplyr)
        y <- data %>% select(fips, Emissions, year) %>% filter(fips == "24510")
        y <- as.data.frame(tapply(y$Emissions, y$year, sum))
        plot(c(1999, 2002, 2005, 2008), y[,1], type = "b", pch = 23, col = "purple", 
             lwd = 2, xlab = "Year", ylab = "Emmissions (tons)", 
             main = "Total Emmissions in Baltimore City, Maryland from PM2.5")
        dev.off()
}