plot3 <- function() {
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
        png("plot3.png", width = 480, height = 480)
        library(dplyr)
        y <- data %>% select(fips, Emissions, type, year) %>% 
                filter(fips == "24510") %>% group_by(type, year) %>% 
                summarize(total = sum(Emissions))
        library(ggplot2)
        plot <- qplot(year, total, data = y, color = type, geom = "line", ylab = "Emmissions (tons)", 
              main = "Total Emmissions by Type in Baltimore City, Maryland from PM2.5")
        print(plot)
        dev.off()
}