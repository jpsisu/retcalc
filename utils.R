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
  inpw$incMonKLKSS <- as.numeric(inpw$incMonKLKSS)
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
#
# inc   Income
# exp   Expense
# mon   Monthly value
# year  Yearly value
#############################################################################################
getValueByMonth <- function(monthStart, initValue, inp) {
  #Get the monthly interest rate
  #browser()
  value <- numeric(length(monthStart))
  growth <- numeric(length(monthStart))
  contributions <- numeric(length(monthStart))  
  incSvgsWdraw <- numeric(length(monthStart))
  #Only add contributions for the number of years working
  monthsFTW <- inp$yearsFTW*12
  wRate <- inp$wRatePerc / 100.
  wRateMonth <- wRate / 12.
  
  preMPR <- inp$preAPR / (100.*12.)
  value[1] <- initValue
  
  #Calculate the amount to withdraw from the first month.  That will get used each
  # subsequent month but possibly inflated.
  
  wdMonthOne <- initValue * wRateMonth
  for (nMonth in 1:(length(monthStart)-1)) {
    #browser()
    growth[nMonth] <- value[nMonth]*preMPR
    contributions[nMonth] <- ifelse(nMonth<=monthsFTW, inp$addMonth + inp$addNORCMonth, 0)
    incSvgsWdraw[nMonth] <- ifelse(nMonth>monthsFTW & value[nMonth]>0, wdMonthOne, 0)    
    value[nMonth+1] <- value[nMonth] + growth[nMonth] + contributions[nMonth] - incSvgsWdraw[nMonth]
    value[nMonth+1] <- ifelse(value[nMonth+1]<0, 0, value[nMonth+1])
  }
  vts <- data.frame(month = monthStart, growth, value, contributions, incSvgsWdraw, year=floor_date(monthStart,unit="year"))
  vts$valueK <- vts$value / 1000.
  vts$valueM <- vts$value / 1000000.
  agg <- vts %>% group_by(year) %>% summarize(growth=sum(growth),
                                              contributions = sum(contributions),
                                              incSvgsWdraw = sum(incSvgsWdraw),
                                              minValue = sum(value))
  #browser()  
  return(vts)

}
#############################################################################################
getValueByYear <- function(vts) {
  agg <- vts %>% group_by(year) %>% summarize(growth=sum(growth),
                                              contributions = sum(contributions),
                                              incSvgsWdraw = sum(incSvgsWdraw),
                                              minValue = min(value),
                                              maxValue = max(value))
  return(agg)
}