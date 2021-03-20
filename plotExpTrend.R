plotExpTrend <- function(dat){
  print("RED IS OLS, BLUE IS WLS")
  fit1 = lm( dat$logAdj ~ dat$Date )
  fit2 = lm( dat$logAdj ~ dat$Date, weights = 1:length(dat$Open) )
  sp <- ggplot(dat, aes(y = Adjusted, x = Date)) + geom_line() + scale_y_continuous(trans='log') + 
    geom_abline(intercept = fit1$coefficients[[1]], slope = fit1$coefficients[[2]], color = "Red", size=1) +
    geom_abline(intercept = fit2$coefficients[[1]], slope = fit2$coefficients[[2]], color = "Blue", size=1) 
  sp
  ## Blå er WLS
}