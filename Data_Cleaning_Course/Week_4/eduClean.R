eduClean <- function(doc = "edu.csv") {
        edu <- read.csv(doc)
        edu <- edu[, c(1, 2, 10)]
        edu[edu==""]<-NA
        edu <- edu[complete.cases(edu),]
        edu
}

#CountryCode, Long.Name, Income.Group