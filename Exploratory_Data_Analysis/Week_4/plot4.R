plot4 <- function() {
        #loads data file onto computer if it isn't already there
        if(!file.exists("plotData.zip")) {
                url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                download.file(url, "plotData.zip")
                unzip("plotData.zip")
        }
        
        #readdata into R
        data <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        #assign souce value to data data.frame
        library(stringr)
        library(dplyr)
        SCC2 <- SCC %>% select(SCC, EI.Sector)
        SCC2$EI.Sector <- word(SCC2$EI.Sector, -1)
        data2 <- data %>% select(fips, SCC, Emissions, year) %>% 
                      mutate(source = SCC2[match(SCC, SCC2$SCC), 2]) 
        
        #Create plot
        png("plot4.png", width = 480, height = 480)
        y <- data2 %>% filter(source == "Coal") %>% group_by(year) %>% 
                summarize(total = sum(Emissions)) 
        library(ggplot2)
        plot <- qplot(year, total, data = y, geom = "line", ylab = "Emmissions (tons)", 
                      xlab = "Year", main = "Total Coal Emmissions in United States from PM2.5")
        print(plot)
        dev.off()
}