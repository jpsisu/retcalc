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
    print("about to plot savings...")
    output$distPlotSavings <- renderPlot({
        logmsg("*****in render plot to distPlotSavings*****")
        inp$preAPR <- input$preAPR
        inp$preMPR <- input$preAPR / (100.*12.) 
        inp$yearsFTW <- input$yearsFTW
        inp$monthsFTW <- inp$yearsFTW*12
        inp$wRatePerc <- input$wRatePerc
        inp$wRate <- inp$wRatePerc / 100.  
        inp$wRateMonth <- inp$wRate / (12)
        inp$inflRate <- input$inflRate        
        inp$inflRateMonth <- inp$inflRate / (12.*100)
        vts <- fillTimeSeries(inp)        
        ggplot(vts, aes(x = month, y = valueK)) + 
            theme_bw() + geom_line() + 
            ggtitle("Retirement Savings\nValue by Month ($1,000)") +
            labs(x = "Month", y = "Value ($1000s)")
#        ggplot(vts, aes(x = month, y = withdrawals)) + 
#            theme_bw() + geom_line() + 
#            ggtitle("Retirement Income\nBy Month") +
#            labs(x = "Month", y = "Value")

    })
    logmsg("about to plot income,,")
    output$distPlotIncome <- renderPlot({
        logmsg("in renderPlot for distPlotIncome ...")
        inp$preAPR <- input$preAPR
        inp$preMPR <- input$preAPR / (100.*12.)      
        inp$yearsFTW <- input$yearsFTW
        inp$monthsFTW <- inp$yearsFTW*12
        inp$wRatePerc <- input$wRatePerc
        
        inp$wRate <- inp$wRatePerc / 100.  
        inp$wRateMonth <- inp$wRate / (12)
        
        inp$inflRate <- input$inflRate
        inp$inflRateMonth <- inp$inflRate / (12.*100)
        vts <- fillTimeSeries(inp) 
        dumpSeriesByYear(vts)
        
        #Aggregate 
        vts_year <- getValueByYear(vts)
        #browser()
        ggplot(vts_year, aes(x = year, y = incSvgsWdraw)) + 
            theme_bw() + geom_line() + 
            ggtitle("Retirement Income\nBy Year") +
            labs(x = "Year", y = "Value")
        
    })    

})
