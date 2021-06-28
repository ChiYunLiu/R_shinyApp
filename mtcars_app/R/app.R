# author name: Chi Yun, Liu
# reference: https://stackoverflow.com/questions/25633662/create-plots-based-on-radio-button-selection-r-shiny

library(shiny)
library(ggplot2)

ui <- fluidPage(
  varSelectInput("xVar", "X variable", data = mtcars),
  radioButtons("plotType", "Choose a plot type", 
               choiceNames = c("Density Plot", "Histogram", "Frequency Polygon"),
               choiceValues = c("DP", "HT", "FP")),
  plotOutput("plot", width = "80%")
)

server <- function(input, output, session) {
  
  ploTyp <- function(x, type){
    switch(type,
           DP = ggplot(mtcars, aes(x = !!input$xVar)) + geom_density(outline.type = "full") + theme_bw(),
           HT = ggplot(mtcars, aes(x = !!input$xVar)) + geom_histogram() + theme_bw(),
           FP = ggplot(mtcars, aes(x = !!input$xVar)) + geom_freqpoly() + theme_bw())
  }
  
  
  output$plot <- renderPlot({
    ploTyp(xVar, input$plotType)
    
    
  })
  
}

shinyApp(ui, server)