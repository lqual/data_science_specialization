library(leaflet)
library(dplyr)
df <- data.frame(lat = runif(200, min = 31.93, max = 36.83), 
                 lng = runif(200, min = -108.98, max = -103.19),
                 type = sample(c("gold", "silver", "diamonds"),
                               200, replace = TRUE),
                 value = round(runif(200, min = 0.25, max = 10), digits = 1))
pal <- colorFactor(palette = c("red", "green", "blue"), 
                   levels = c("gold", "silver", "diamonds"))
df %>% 
        leaflet() %>% 
        addTiles() %>%
        addCircles(weight = 1, radius = df$value * 1000, color = pal(df$type), 
                   popup = paste("$", df$value, "million")) %>%
        addLegend(labels = c("Gold", "Silver", "Diamonds"),
                  colors = c("red", "green", "blue"))
