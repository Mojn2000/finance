getDat <- function(ticker){
  rawDat = getSymbols(ticker,src="yahoo", from = "1950-01-01", auto.assign = F, period = "Max")
  rawDat = na.approx(rawDat)
  dat <- as.data.frame(rawDat)
  dat$date <- time(rawDat)
  names(dat) = c("Open","High","Low","Close","Vol","Adjusted","Date")
  dat$logAdj = log(dat$Adjusted)
  return( dat )
}