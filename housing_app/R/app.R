# author name: Chi Yun, Liu

# reference:
# https://stackoverflow.com/questions/36366162/shiny-how-to-embed-a-sidebarpanel-inside-tabpanel
# https://stackoverflow.com/questions/44222796/shiny-with-multiple-tabs-and-different-sidebar-in-each-tab
# https://stackoverflow.com/questions/4973898/combining-paste-and-expression-functions-in-plot-labels

library(shiny)
library(tidyverse)
library(broom)

# Load and tidy data
estate <- read_csv("../data/estate.csv")
estate %>% 
    mutate(AC = recode(AC,
                       `1` = "AC",
                       `0` = "No AC"),
           Pool = recode(Pool,
                         `1` = "Pool",
                         `0` = "No Pool"),
           Highway = recode(Highway,
                            `1` = "HW",
                            `0` = "No HW"),
           Price = Price/1000,
           Style = as.character(Style)) %>%
    rename(`Price($K)` = Price) -> estate01

# Define ui
ui <- fluidPage(
    titlePanel("EDA of Estate Data"),
    tabsetPanel(
        tabPanel("Univariate",
                sidebarLayout(
                    sidebarPanel(
                        varSelectInput("var1", "Variable?", selected = "Price($K)", data = estate01),
                        checkboxInput("logX", "Log...Transform?"),
                        sliderInput("bins", "Number of Bins?", min = 1, max = 100, value = 10),
                        numericInput("num", "Null Value", value = 0), 
                        tableOutput("ttest"),
                    ),
                    mainPanel(
                        plotOutput("plotUn")
                    )
                )
         ),
        tabPanel("Bivariate",
                 sidebarLayout(
                     sidebarPanel(
                         varSelectInput("var2", "X Variable?", selected = "Area", data = estate01),
                         checkboxInput("logXX", "Log...Transform?"),
                         varSelectInput("var3", "Y Variable?", selected = "Price($K)", data = estate01),
                         checkboxInput("logY", "Log...Transform?"),
                         checkboxInput("ols", "Fit OLS?")
                     ),
                     mainPanel(
                         plotOutput("plotBi")
                     )
                 ),
                 fluidRow(column(4, verbatimTextOutput("summary")),
                          column(4, plotOutput("residPlot")),
                          column(4, plotOutput("qqPlot"))
                 )
        ),
        tabPanel("Spreadsheet",
             dataTableOutput("dynamic")
             )
    )
)

# assign output 
server <- function(input, output, session) {
# tab1   
   output$plotUn <- renderPlot({
      p1 <- ggplot(estate01, aes(x = !!input$var1))
   if(is.numeric(estate01[[input$var1]])){
      if(input$logX){
         p1 <- p1 + geom_histogram(bins = input$bins) + scale_x_log10 () + xlab(paste("Log(", input$var1, ")")) # plot with log
      } else {
         p1 <- p1 + geom_histogram(bins = input$bins) # numeric
      } 
   } else {
      if(input$logX){
         p1 <- p1 + geom_bar() + scale_x_log10() # plot with log
         validate(
            need(is.numeric(estate01[[input$var1]]), "log transform is invalid") # warning message
         )
      } else { 
         p1 <- p1 + geom_bar()} # categorical
   }
   p1

})
   # tab1 t.test 
    output$ttest <- renderTable({
       if (is.numeric(estate01[[input$var1]])){
          if(input$logX){
             estate01 %>% 
                select(input$var1) %>% 
                log() %>% 
                t.test(mu = input$num) %>% 
                broom::tidy() %>%
                select("P-value" = p.value, "Estimate" = estimate, "95 % Lower" = conf.low, "95 % Upper" = conf.high)
          } else {
             estate01 %>% 
                select(input$var1) %>% 
                t.test(mu = input$num) %>% 
                broom::tidy() %>% 
                select("P-value" = p.value, "Estimate" = estimate, "95 % Lower" = conf.low, "95 % Upper" = conf.high)
          }
       } else {
         validate(
           need(is.numeric(estate01[[input$var1]]), "Variable is not numeric"))
       }
        })
 # tab 2
    output$plotBi <- renderPlot({
      p2 <- ggplot(estate01, aes(x = !!input$var2, !!input$var3))
      # Both numeric
      if(is.numeric(estate01[[input$var2]]) & is.numeric(estate01[[input$var3]])){
        if (input$logXX){
            p2 <- p2 + geom_point() + scale_x_log10() + xlab(paste("Log(", input$var2, ")"))
          } else {
          p2 <- p2 + geom_point()  #log X
          }
        if (input$logY){ #log Y
          p2 <- p2 + geom_point() + scale_y_log10() + ylab(paste("Log(", input$var3, ")")) 
        } else{
          p2 <- p2 + geom_point()
        }
        if (input$logXX & input$logY){ #log X and log Y
          p2 <- p2 + geom_point() + scale_x_log10() + scale_y_log10() + xlab(paste("Log(", input$var2, ")")) + ylab(paste("Log(", input$var3, ")")) 
        } else {
          p2 <- p2 + geom_point() 
        }
        if (input$ols) { # assign OLS
          p2 <- p2 + geom_point() + geom_smooth(method = lm, se = FALSE)
          output$summary <- renderPrint({
            lmout <- lm(log(estate01[[input$var3]]) ~ log(estate01[[input$var2]]))
            print(summary(lmout))
          })
          output$residPlot <- renderPlot({
            lmout <- lm(log(estate01[[input$var3]]) ~ log(estate01[[input$var2]]))
            broom::augment(lmout) %>%
            ggplot(aes(x = .fitted, y = .resid)) +
              geom_point() +
              labs(title = "Residuals vs Fitted", x = "x", y = "y")
          })
          output$qqPlot <- renderPlot({ # QQ plot
            model_reg <- lm(log(estate01[[input$var3]]) ~ log(estate01[[input$var2]]))
            broom::augment(model_reg) %>%
            ggplot(aes(sample = .resid)) +
              stat_qq() +
              stat_qq_line() +
              labs(title = "QQ Plot")
          })
          } else {
            p2 <- p2 + geom_point()
          }
    
        # both categorical
      } else if (is.character(estate01[[input$var2]]) & is.character(estate01[[input$var3]])) {
        if (input$logXX){
          validate(
            need(is.numeric(estate01[[input$var2]]), "Variables are not numeric"))
        } else {
          p2 <- p2 + geom_jitter()
        }
        if (input$logY) {
          validate(
            need(is.numeric(estate01[[input$var3]]), "Variables are not numeric"))
        } else {
          p2 <- p2 + geom_jitter()
        } 
        if (input$logXX & input$logY){
          validate(
            need(is.numeric(estate01[[input$var2]]) & is.numeric(estate01[[input$var3]]), "Variables are not numeric"))
        } else {
          p2 <- p2 + geom_jitter()
        }
        # x numeric with a y categorical    
      } else if (is.numeric(estate01[[input$var2]]) & is.character(estate01[[input$var3]])){
        if (input$logXX){ # log x
          p2 <- p2 + geom_boxplot() + scale_x_log10() + xlab(paste("Log(", input$var2, ")")) 
        } else {
          p2 <- p2 + geom_boxplot() 
        }
        if (input$logY){
          validate(
            need(is.numeric(estate01[[input$var3]]), "Y Variable is not numeric"))
        } else {
          p2 <- p2 + geom_boxplot()
        }
        if (input$logXX & input$logY){
          validate(
            need(is.numeric(estate01[[input$var2]]) & is.numeric(estate01[[input$var3]]), "Variables are not numeric"))
        } else {
          p2 <- p2 + geom_boxplot()
        }
        # x categorical with a y numeric 
      } else {
        if (input$logXX){
          validate(
            need(is.numeric(estate01[[input$var2]]), "X Variable is not numeric"))
        } else {
          p2 <- p2 + geom_boxplot()
        }
        if (input$logY){ # log Y
          p2 <- p2 + geom_boxplot() + scale_y_log10() + ylab(paste("Log(", input$var3, ")")) 
        } else {
          p2 <- p2 + geom_boxplot()
        }
        if (input$logXX & input$logY){
          validate(
            need(is.numeric(estate01[[input$var2]]) & is.numeric(estate01[[input$var3]]), "Variables are not numeric"))
        } else {
          p2 <- p2 + geom_boxplot()
        }
        }
      p2
})
       
      
# tab 3 spreadsheet 
       output$dynamic <- renderDataTable({
          estate01 %>% 
             keep(~typeof(.) == "double")
       }, options = list(pageLength = 10))
       
}



shinyApp(ui = ui, server = server)


