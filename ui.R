#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#https://www.showmeshiny.com/predictive-value/

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
              column(4,
                     sliderInput("preAPR",
                                 "Interest APR",
                                 min = 0,
                                 max = 20,
                                 value = 5)
            ),
            column(4,            
            sliderInput("yearsFTW",
                        "Years of Full Time Work",
                        min = 0,
                        max = 5,
                        value = 3)
        ),
        
#        hr(),
        fluidRow(
            column(4,            
            sliderInput("wRatePerc",
                        "Widthdrawal Rate (%)",
                        min = 0,
                        max = 10,
                        value = 3) 
            ),
            column(4,            
                   sliderInput("inflationRate",
                               "Inflation Rate (%)",
                               min = 0,
                               max = 6,
                               value = 2) 
            )            
        ),
        
 #       strong("Caption:"),
        
#        p("This widget is meant to show the effect that changing clinical test accuracy, precision, sensitivity, and specificity has on other variables. You have to select whether to manipulate the characteristic statistics of the test (sensitivity and specificity) or the direct quantities (non-disease and disease mean and standard deviation, and the cutoff for a positive test). Whichever set you want to manipulate using the sliders, the other quantities will be calculated. More information is available at the bottom of this page."),
#        hr(),
        br(),
        br()
    )
))

