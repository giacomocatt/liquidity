library("readr")
library("tidyr")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
library(pbapply)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

y_curve <- read.csv('yield_curve.csv')
y_curve$Date <- as.Date(y_curve$Date, '%m/%d/%y')
y_curve <- y_curve[!is.na(y_curve$X1.Mo),]
y_curve <- y_curve[y_curve$Date>= '2019-01-01' & y_curve$Date<= '2022-12-31' ,]

mat_treas <- c(1/12, 2/12, 3/12, 4/12, 6/12, 1,2,3,5,7,10,20,30)
#y_curve <- y_curve[,-1]
library(xts)
ycurve <- xts(y_curve[,-1], order.by = y_curve$Date)
library(YieldCurve)
ns_coeffs <- Nelson.Siegel(ycurve,mat_treas)
ns_coeffs <- read.csv('ns_coeffs.csv')
ns_coeffs$Date <- as.Date(ns_coeffs$Date)
ns_coeffs <- xts(ns_coeffs[,!names(ns_coeffs) == 'Date'], order.by = ns_coeffs$Date)

rf <- function(t, tau) {
  ns <- as.numeric(ns_coeffs[t,])
  tau <- tau/12
  y <- ns[1] + ns[2] *(1-exp(-ns[4]*tau))/(ns[4]*tau) + ns[3] *((1-exp(-ns[4]*tau))/(ns[4]*tau) - exp(-ns[4]*tau))
  y
}
df$rf <- pbmapply(rf, df[, 'trd_exctn_dt'] , df[,'ttm_agg'])
df$cs <- df$yield - df$rf
df_ <- df[, c(1,2,6,7,8,9,10,27,28)]

ret <- read.csv('Bond_Ret_WRDS.csv')
ret <- ret[ret$SECURITY_LEVEL == 'SEN' & ret$CONV == 0,]
ret$ttm <- difftime(ret$MATURITY , ret$DATE, units = 'days')

ret_x <- merge(ret, ns_coeffs, by.x='DATE', by.y='Date')
ret_x$ttm <- ret_x$ttm/(12*30)
ret_x$ttm <- as.numeric(ret_x$ttm)
ret_x$rff <- ret_x$beta_0 + ret_x$beta_1 *(1-exp(-ret_x$lambda*ret_x$ttm))/(ret_x$lambda*ret_x$ttm) + 
  ret_x$beta_2 *((1-exp(-ret_x$lambda*ret_x$ttm))/(ret_x$lambda*ret_x$ttm) - exp(-ret_x$lambda*ret_x$ttm))
ret_x$cs <- ret_x$T_Yld_Pt- ret_x$rff
ret_x <- ret_x[!is.na(ret_x$cs), !names(ret_x) %in% c('bsym', 'SECURITY_LEVEL', 'CONV')]

ret_x <- read.csv('Bond_returns.csv')
shock <- read.csv('unc_shock_monthly.csv')
xx <- merge(ret_x, shock, by.x="DATE", by.y='date')

firms <- read_csv('fin_ratios_compustat.csv')
firms$ISSUER_CUSIP <- strtrim(firms$cusip, 6)
ret_x$ISSUER_CUSIP <- strtrim(ret_x$CUSIP, 6)
ret_x <- as.data.table(ret_x)
firms <- as.data.table(firms)
xx <- merge(bonds, df, by.x = c('DATE', 'ISSUER_CUSIP'), by.y = c('public_date', 'ISSUER_CUSIP'))

data <- read_dta('callreports_1976_2020_WRDS.dta')
df <- data[, names(data) %in% c('date', 'name', 'assets', 'equity', 'fedfundsrepoasset', 'fedfundsrepoliab', 'securities', 'ciloans', 'intincciloans', 'intincassets')]
df$leverage <- df$assets/(df$assets - df$equity)
