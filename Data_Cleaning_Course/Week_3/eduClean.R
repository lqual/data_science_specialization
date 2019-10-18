eduClean <- function(doc = "edu.csv") {
        edu <- read.csv(doc)
        edu <- edu[, 1:3]
        edu[edu==""]<-NA
        edu <- edu[complete.cases(edu),]
        edu
}

#CountryCode, Long.Name, Income.Group