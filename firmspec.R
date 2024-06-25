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

df_agg$month <- ifelse(df_agg$quarter=='Q1', '01', ifelse(df_agg$quarter=='Q2', '04', ifelse(df_agg$quarter=='Q3', '07', ifelse(df_agg$quarter=='Q4', '10', 'NA'))))
df_agg$date <- with(df_agg,paste(year,month,'01', sep="-"))
df_agg$date <- as.Date(df_agg$date,format="%Y-%m-%d")
df_ags <- df_agg[,-c(1,2,9)]
data <- split(df_ags, df_ags$ISSUER_CUSIP.x)
lags <- sapply(data, 
               function(x) {
                 x <- as.data.frame(x)
                 x <- xts(x[,-7], order.by = as.Date(x$date))
                 x$debt_at_lag = lag(x$debt_at)
                 x$debt_capital_lag = lag(x$debt_capital)
                 x$opmad_lag = lag(x$opmad)
                 x$cash_debt_lag = lag(x$cash_debt)
                 x})
lagss <- lapply(lags, 
                function(x){
                  y = data.frame(date =index(x), coredata(x))
                  y})
lagss<-list_rbind(lagss)
lagss$quarter <- quarters(lagss$date)
lagss$year <- format(lagss$date, '%Y')

lagss$quarter <- quarters(lagss$public_date)
lagss$year <- format(lagss$public_date, '%Y')
lagss <- as.data.table(lagss)

df_or <- df_or[,names(df_or) %in% c("COMPLETE_CUSIP.x" , "ISSUER_CUSIP.x", "RATING")]
bondss <- merge(bonds, df_or, by.x = 'cusip_id', by.y = "COMPLETE_CUSIP.x")
data <- merge(bondss, lagss, by = c('quarter', 'year', 'ISSUER_CUSIP.x'))
