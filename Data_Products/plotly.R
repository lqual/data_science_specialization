#histogram
library(plotly)
plot_ly(x = mtcars$mpg, type = "histogram") %>% 
        layout(title = "MPG Rates in Mtcars Dataset",
               xaxis = list(title = "MPG"),
               yaxis = list(title = "# of occurences"))

#scatterplot
library(plotly)
plot_ly(x = mtcars$hp, y = mtcars$mpg, mode = "markers",
        size = mtcars$wt, color = as.factor(mtcars$cyl),
        text = paste("Weight: ", mtcars$wt)) %>% 
        layout(title = "MPG Rates vs Horsepower, Cylinders, and Weight",
               xaxis = list(title = "Horsepower"),
               yaxis = list(title = "MPG"))





