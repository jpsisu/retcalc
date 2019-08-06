library(openxlsx)
library(tidyverse)
library(lubridate)
setwd("C:\\Users\\patri\\Documents\\projects\\RetirementModel\\retcalc")
#############################################################################################
readSetup <- function() {
  fname <- "RetirementSetup.xlsx"
  inp <- read.xlsx(fname, sheet = "inputs")
#  browser()
  #Easier to update in long format but use in wide format
  inpw <- inp %>% spread(desc,value)
  inpw$startDate <- as.Date(inpw$startDate,format="%m/%d/%Y")
  inpw$initPreTax <- as.numeric(inpw$initPreTax)
  inpw$initPostTax <- as.numeric(inpw$initPostTax)  
  inpw$preAPR <- as.numeric(inpw$preAPR)
  inpw$postAPR <- as.numeric(inpw$postAPR)  
  inpw$addMonth <- as.numeric(inpw$addMonth)
  inpw$yearsFTW <- as.numeric(inpw$yearsFTW)
  inpw$wRatePerc <- as.numeric(inpw$wRatePerc)  
  inpw$addNORCMonth <- as.numeric(inpw$addNORCYear) / 12.
  return(inpw)
}
#############################################################################################
fillTimeSeries <- function(inp) {
  initValue <- inp$initPostTax + inp$initPreTax
  
  #Project out a certain number of years
  #browser()
  monthStart <- seq(inp$startDate,inp$startDate+years(30),by="month")
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
  contributions <- numeric(length(monthStart))  
  withdrawals <- numeric(length(monthStart))
  #Only add contributions for the number of years working
  monthsFTW <- inp$yearsFTW*12
  wRate <- inp$wRatePerc / 100.
  wRateMonth <- wRate / 12.
  
  preMPR <- inp$preAPR / (100.*12.)
  value[1] <- initValue
  for (nMonth in 1:(length(monthStart)-1)) {
    #browser()
    growth[nMonth] <- value[nMonth]*preMPR
    contributions[nMonth] <- ifelse(nMonth<=monthsFTW, inp$addMonth + inp$addNORCMonth, 0)
    withdrawals[nMonth] <- ifelse(nMonth>monthsFTW, value[nMonth]*wRateMonth, 0)    
    value[nMonth+1] <- value[nMonth] + growth[nMonth] + contributions[nMonth] - withdrawals[nMonth]
  }
  #browser()
  vts <- data.frame(month = monthStart, growth, value, contributions, withdrawals)
  vts$valueK <- vts$value / 1000.
  vts$valueM <- vts$value / 1000000.
  #browser()  
  return(vts)

}