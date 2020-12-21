library(IMFData)
library(tidyverse)
library(lubridate)
library(plm)
library(lmtest)
library(wbstats)
library(Hmisc)
library(corrplot)
library(quantmod)
expand_data <- function(x) {
  years <- min(x$date):max(x$date)
  quarters <- 1:4
  grid <- expand.grid(quarter=quarters, date=years)
  x$quarter <- 1
  merged <- grid %>% left_join(x, by=c('date', 'quarter'))
  merged$country <- x$country[1]
  return(merged)
}
interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$mobile_subscriptions
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$mobile_subscriptions_int <- interpolation$y
  return(data)
}

