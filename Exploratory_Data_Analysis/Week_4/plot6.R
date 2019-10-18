plot6 <- function() {
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
        
        #create Baltimore City data
        balt <- data2 %>% filter(source == "Vehicles" & fips == "24510") %>% 
                group_by(year) %>% summarize(total = sum(Emissions)) 
        y.balt <- balt %>% pull(total)
        x.balt <- balt %>% pull(year)
        
        #create Los Angeles County data
        lac <- data2 %>% filter(source == "Vehicles" & fips == "06037") %>% 
                group_by(year) %>% summarize(total = sum(Emissions)) 
        y.lac <- lac %>% pull(total)
        x.lac <- lac %>% pull(year)
        
        #Create plot
        png("plot6.png", width = 960, height = 480)
        par(mfrow = c(1,2))
        y.range <- range(c(y.balt, y.lac))
        y.range[1] <- 0
        y.range[2] <- round(y.range[2]+100,-2)
        plot(x.balt, y.balt, type = "b", pch = 24, col = "purple", 
             lwd = 2, xlab = "Year", ylab = "Emmissions (tons)", 
             main = "Vehicle Emmissions in Baltimore City", ylim = y.range)
        plot(x.lac, y.lac, type = "b", pch = 24, col = "blue", 
             lwd = 2, xlab = "Year", ylab = "Emmissions (tons)", 
             main = "Vehicle Emmissions in Los Angeles County", ylim = y.range)
        dev.off()
}