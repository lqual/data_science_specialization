#download the datafile
if(!file.exists("stormData.csv.bz2")) {
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(url, "stormData.csv.bz2")
}
#read the datafile and remove unneeded columns
library(data.table)
library(R.utils)
data <- fread("stormData.csv.bz2")
library(dplyr)
data2 <- data %>% select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, 
                         PROPDMGEXP, CROPDMG, CROPDMGEXP)

#convert BGN_DATE to date
library(lubridate)
data2$BGN_DATE <- ymd(mdy_hms(data2$BGN_DATE))

#for EVTYPE, will only filter out summary values
#for the purposes of the assignment, we will use the National Weather Service's 
#classifications as correct instead of regrouping event types together
#also will not attempt to correct typos for EVTYPE, since our analysis will
#be using averages for impact damage and reclassifying the data incorrectly
#could cause more harm then good
data2 <- data2 %>% filter(!grepl("^[Ss][Uu][Mm]", EVTYPE))

#creating a total people harmed variable
data2 <- data2 %>% mutate(people_harmed = FATALITIES + INJURIES)

#for property damage and crop damage variables, the exponential variable gives
#the multiplier (h = hundreds, k = thousands, m = millions, b = billions,
#any number = 10 + number).  All other symbols will be disregarded and damage
#value will be used
data2 <- data2 %>% mutate(PROPDMGEXP = 
                                ifelse(PROPDMGEXP == "h", "H", 
                                ifelse(PROPDMGEXP == "k", "K", 
                                ifelse(PROPDMGEXP == "m", "M", 
                                ifelse(PROPDMGEXP == "b", "B", PROPDMGEXP)))))

data2 <- data2 %>% mutate(property = 
                                ifelse(PROPDMGEXP == "H", PROPDMG * 100, 
                                ifelse(PROPDMGEXP == "K", PROPDMG * 1000,
                                ifelse(PROPDMGEXP == "M", PROPDMG * 1000000,
                                ifelse(PROPDMGEXP == "B", PROPDMG * 1000000000,
                                ifelse(PROPDMGEXP == "0", PROPDMG * 10,
                                ifelse(PROPDMGEXP == "1", PROPDMG * 10 + 1,
                                ifelse(PROPDMGEXP == "2", PROPDMG * 10 + 2,
                                ifelse(PROPDMGEXP == "3", PROPDMG * 10 + 3,
                                ifelse(PROPDMGEXP == "4", PROPDMG * 10 + 4,
                                ifelse(PROPDMGEXP == "5", PROPDMG * 10 + 5,
                                ifelse(PROPDMGEXP == "6", PROPDMG * 10 + 6,
                                ifelse(PROPDMGEXP == "7", PROPDMG * 10 + 7,
                                ifelse(PROPDMGEXP == "8", PROPDMG * 10 + 8,
                                ifelse(PROPDMGEXP == "9", PROPDMG * 10 + 9,
                                       PROPDMG)))))))))))))))

#now doing crop damage
data2 <- data2 %>% mutate(CROPDMGEXP = 
                                ifelse(CROPDMGEXP == "h", "H", 
                                ifelse(CROPDMGEXP == "k", "K", 
                                ifelse(CROPDMGEXP == "m", "M", 
                                ifelse(CROPDMGEXP == "b", "B", CROPDMGEXP)))))

data2 <- data2 %>% mutate(crop = 
                                ifelse(CROPDMGEXP == "H", CROPDMG * 100, 
                                ifelse(CROPDMGEXP == "K", CROPDMG * 1000,
                                ifelse(CROPDMGEXP == "M", CROPDMG * 1000000,
                                ifelse(CROPDMGEXP == "B", CROPDMG * 1000000000,
                                ifelse(CROPDMGEXP == "0", CROPDMG * 10,
                                ifelse(CROPDMGEXP == "1", CROPDMG * 10 + 1,
                                ifelse(CROPDMGEXP == "2", CROPDMG * 10 + 2,
                                ifelse(CROPDMGEXP == "3", CROPDMG * 10 + 3,
                                ifelse(CROPDMGEXP == "4", CROPDMG * 10 + 4,
                                ifelse(CROPDMGEXP == "5", CROPDMG * 10 + 5,
                                ifelse(CROPDMGEXP == "6", CROPDMG * 10 + 6,
                                ifelse(CROPDMGEXP == "7", CROPDMG * 10 + 7,
                                ifelse(CROPDMGEXP == "8", CROPDMG * 10 + 8,
                                ifelse(CROPDMGEXP == "9", CROPDMG * 10 + 9,
                                        CROPDMG)))))))))))))))

#filter our 0's for property and crop to cut down on number of observations
data2 <- data2 %>% mutate(damage = property + crop) %>% filter(damage != 0) 
