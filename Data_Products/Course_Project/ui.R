library(shiny)
shinyUI(fluidPage(
        titlePanel("Predicting Black Cherry Tree Volume and Height"),
        sidebarLayout(
                sidebarPanel(
                h1("What is the diameter of the tree?"),
                sliderInput(
                        "slider", "Diameter in Inches",
                        value = mean(trees$Girth), min = 8, max = 21, step = 0.1
                ),
                h4("Measure tree diameter in inches at 4 ft 6 in above the ground.")
                ),
                mainPanel(
                        plotOutput("plotV"),
                        h3("Predicted Volume (cubic feet):"),
                        textOutput("predV"),
                        plotOutput("plotH"),
                        h3("Predicted Height (feet):"),
                        textOutput("predH")
                )
        )        
))