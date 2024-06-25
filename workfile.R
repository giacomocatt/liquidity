library("readr")
library("tidyr")
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

df <- read_csv('fin_ratios_compustat.csv')
df <- df[,names(df) %in% c('public_date', 'cusip', 'debt_at', 'debt_capital', 'opmad', 'cash_debt')]
bonds <- read_csv('liq_meas_cs_iss.csv')
df$ISSUER_CUSIP.x <- strtrim(df$cusip, 6)
df$quarter <- quarters(df$public_date)
df$year <- format(df$public_date, '%Y')
bonds$quarter <- quarters(bonds$trd_exctn_dt)
bonds$year <- format(bonds$trd_exctn_dt, '%Y')
bonds <- as.data.table(bonds)
df <- as.data.table(df)
detach(package:plyr)
df_agg <- df %>%
  group_by(year, quarter, cusip, ISSUER_CUSIP.x) %>%
  summarize(opmad = mean(opmad, na.rm=T), debt_at = mean(debt_at, na.rm=T), cash_debt = mean(cash_debt, na.rm=T), debt_capital = mean(debt_capital, na.rm=T))
data <- merge(bonds, df_agg, by = c('quarter', 'year', 'ISSUER_CUSIP.x'))

df_agg$month <- ifelse(df_agg$quarter=='Q1', 1, ifelse(df_agg$quarter=='Q2', 4, ifelse(df_agg$quarter=='Q3', 7, ifelse(df_agg$quarter=='Q4', 10, 'NA'))))
df_agg$date <- as.Date(with(df_agg,paste(year,month,sep="-")),"%Y-%m")
df_ags <- df_agg[,-c(1,2,10)]
data <- split(df_ags, df_ags$ISSUER_CUSIP.x)
lags <- sapply(data, 
               function(x) {
                 x <- xts(x[,-7], order.by = as.Date(x$date))
                 x$debt_at_lag = lag(x$debt_at)
                 x$debt_capital = lag(x$debt_capital)
                 x$opmad_lag = lag(x$opmad)
                 x$cash_debt = lag(x$cash_debt)
                 x})
lagss <- lapply(lags, 
                function(x){
                  y = data.frame(date =index(x), coredata(x))
                  y})
lagss<-list_rbind(lagss)
lagss$quarter <- quarters(lagss$date)
lagss$year <- format(lagss$date, '%Y')