library(openxlsx)
library(tidyverse)
library(lubridate)
setwd("C:\\Users\\patri\\Documents\\projects\\RetirementModel\\retcalc")
#############################################################################################
readSetup <- function() {
  fname <- "RetirementSetup.xlsx"
  inp <- read.xlsx(fname, sheet = "inputs")
  #Easier to update in long format but use in wide format
  inpw <- inp %>% spread(desc,value)
  inpw$startDate <- as.Date(inpw$startDate,format="%m/%d/%Y")
  inpw$initPreTax <- as.numeric(inpw$initPreTax)
  inpw$initPostTax <- as.numeric(inpw$initPostTax)  
  inpw$preAPR <- as.numeric(inpw$preAPR)
  inpw$postAPR <- as.numeric(inpw$postAPR)  
  return(inpw)
}
#############################################################################################
fillTimeSeries <- function(inp) {
  initValue <- inp$initPostTax + inp$initPreTax
  
  #Project out a certain number of years
  #browser()
  monthStart <- seq(inp$startDate,inp$startDate+years(5),by="month")
  #Now calculate value at the start of each month
  vts <- getValueByMonth(monthStart,initValue, inp)
  return(vts)
  #browser()
}
#############################################################################################
getValueByMonth <- function(monthStart, initValue, inp) {
  #Get the monthly interest rate
  #browser()
  value <- numeric(length(monthStart))
  growth <- numeric(length(monthStart))
  preMPR <- inp$preAPR / (100.*12.)
  value[1] <- initValue
  for (nMonth in 1:(length(monthStart)-1)) {
    #browser()
    growth[nMonth] <- value[nMonth]*preMPR
    value[nMonth+1] <- value[nMonth]+growth[nMonth]
  }
  vts <- data.frame(month = monthStart,growth,value)
  vts$valueK <- vts$value / 1000.
  vts$valueM <- vts$value / 1000000.
  return(vts)
  #browser()
}