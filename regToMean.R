regToMean <- function (dat){
  fit <- lm(dat$logAdj ~ dat$Date)
  return ( (-pnorm(tail(fit$residuals,1)[[1]], sd = sd(fit$residuals)) + 0.5) * 2 )
}
