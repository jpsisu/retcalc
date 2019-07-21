#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("utils.R")
inp <- readSetup()
vts <- fillTimeSeries(inp)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    vts <- fillTimeSeries(inp)
    output$distPlot <- renderPlot({
        inp$preAPR <- input$preAPR
        vts <- fillTimeSeries(inp)        
        ggplot(vts, aes(x = month, y = valueK)) + 
            theme_bw() + geom_line() +
            ggtitle("Retirement Savings\nValue by Month ($1,000)") +
            labs(x = "Month", y = "Value ($1000s)")

    })

})
