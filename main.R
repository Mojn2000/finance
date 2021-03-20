## Install dependencies
library(quantmod)
library(ggplot2)


## 
setwd("/Users/mads/Google Drev/Finance/rDev")
source("getDat.R")
source("regToMean.R")
source("regToMeanW.R")
source("plotExpTrend.R")


tick = "BABA"
dat = getDat( tick )
plotExpTrend( dat )
regToMean( dat )
regToMeanW( dat )

tail(dat)

