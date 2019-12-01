#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#https://www.showmeshiny.com/predictive-value/
#' TODO
#' Interest Rate not working
#'   Key Behavior
#'   server.R
#'      readSetup()
#'         reads in retirementsetup.xlsx and stores the results in a variable called inp
#'      fillTimeSeries()
#'         Projects out a certain number of months, and gets the value by month based on starting value and 
#'         input parameters

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        # Application title
        title = "Retirement Calculations",
        
        titlePanel("Retirement Projections"),
        fluidRow(column(6, plotOutput('distPlotSavings')), column(6, plotOutput('distPlotIncome'))),
        
        br(),
        
        fluidRow(
            column(4, sliderInput("preAPR", "Interest APR", min = 0, max = 10, value = 5)),
            column(4, sliderInput("yearsFTW", "Years of Full Time Work", min = 0, max = 5, value = 3)),
            column(4, sliderInput("wRatePerc", "Widthdrawal Rate (%)", min = 0, max = 10, value = 3)),
            column(4, sliderInput("inflRate", "Inflation Rate (%)", min = 0, max = 6, value = 2))            
        )            
        )
)

