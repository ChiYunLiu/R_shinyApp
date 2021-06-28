# author name: Chi Yun, Liu

library(shiny)
library(tidyverse)

mpg_color <- mpg %>% keep(~typeof(.) == "character")

ui <- fluidPage(
    varSelectInput("xVar", "X variable", selected = "cty", data = mpg),
    varSelectInput("yvar", "Y variable", selected = "hwy", data = mpg),
    varSelectInput("colVar", "Color variable (categorical)",selected = "class", data = mpg_color),
    plotOutput("plot", width = "80%")
  
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        ggplot(mpg, aes(x = !!input$xVar, y = !!input$yvar, color = !!input$colVar)) +
            geom_point() +
            theme_bw()
    })
  
}

shinyApp(ui, server)