gdpClean <- function(doc = "gdp.csv") {
        gdp <- read.csv(doc)
        gdp <- gdp[-c(1:4),]
        gdp <- gdp[, -c(3, 6:10)]
        gdp <- rename(gdp, c("X" = "CountryCode", 
                               "Gross.domestic.product.2012" = "GDP",
                               "X.2" = "Country", "X.3" = "Economy"))
        gdp2 <- gdp[1:190,]
        gdp2[, 2] <- sapply(gdp2[, 2], as.character)
        gdp2[, 2] <- sapply(gdp2[, 2], as.numeric)
        gdp2
}
       