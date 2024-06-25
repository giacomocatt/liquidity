library("readr")
library("tidyr")
library(dplyr)
library(ggplot2)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

df_origin <- read.csv('Issue_&_rating.csv')

cusips <- pull(df_origin, COMPLETE_CUSIP.x)
parts <- split(cusips, rep(1:10, length.out = length(cusips)))

library(RPostgres)
library(devtools)
source_gist("3a05b3ab281563b2e94858451c2eb3a4")

wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = "giacomocattelan",
  password = "PSherman42wallabyway")

trace <- clean_enhanced_trace(
  cusips = parts[[1]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd("2023-08-01"))

trace2 <- clean_enhanced_trace(
  cusips = parts[[2]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace <- rbind(trace, trace2)

trace3 <- clean_enhanced_trace(
  cusips = parts[[3]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace <- rbind(trace, trace3)

trace4 <- clean_enhanced_trace(
  cusips = parts[[4]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace <- rbind(trace, trace4)

trace5 <- clean_enhanced_trace(
  cusips = parts[[5]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace <- rbind(trace, trace5)

trace11 <- clean_enhanced_trace(
  cusips = parts[[6]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))

trace7 <- clean_enhanced_trace(
  cusips = parts[[7]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace11 <- rbind(trace11, trace7)

trace8 <- clean_enhanced_trace(
  cusips = parts[[8]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace11 <- rbind(trace11, trace8)

trace9 <- clean_enhanced_trace(
  cusips = parts[[9]],
  connection = wrds,
  start_date = ymd("2019-01-01"),
  end_date = ymd(Sys.Date()))
trace11 <- rbind(trace11, trace9)

trace10 <- clean_enhanced_trace(
      cusips = parts[[10]],
      connection = wrds,
      start_date = ymd("2019-01-01"),
      end_date = ymd(Sys.Date()))
trace11 <- rbind(trace11, trace10)
df <- rbind(trace, trace11)

df_or <- df_origin[, names(df_origin) %in% c('COMPLETE_CUSIP.x', 'ISSUER_CUSIP.x', 'MATURITY.x', 'OFFERING_DATE.x', 'OFFERING_PRICE', 'OFFERING_YIELD', 'COUPON', 'RATING_TYPE', 'RATING')]
df <- merge(trace, df_or, by.x ='cusip_id', by.y = 'COMPLETE_CUSIP.x')
df$rtng <- ifelse(df$RATING == 'Aaa', 'AAA', NA)
df$rtng <- ifelse(df$RATING == 'Aa1', 'AA+', df$rtng)
df$rtng <- ifelse(df$RATING == 'Aa2', 'AA', df$rtng)
df$rtng <- ifelse(df$RATING == 'Aa3', 'AA-', df$rtng)
df$rtng <- ifelse(df$RATING == 'A1', 'A+', df$rtng)
df$rtng <- ifelse(df$RATING == 'A2', 'A', df$rtng)
df$rtng <- ifelse(df$RATING == 'A3', 'A-', df$rtng)
df$rtng <- ifelse(df$RATING == 'Baa1', 'BBB+', df$rtng)
df$rtng <- ifelse(df$RATING == 'Baa2', 'BBB', df$rtng)
df$rtng <- ifelse(df$RATING == 'Baa3', 'BBB-', df$rtng)
df$rtng <- ifelse(df$RATING == 'Ba1', 'BB+', df$rtng)
df$rtng <- ifelse(df$RATING == 'Ba2', 'BB', df$rtng)
df$rtng <- ifelse(df$RATING == 'Ba3', 'BB-', df$rtng)
df$rtng <- ifelse(df$RATING == 'B1', 'B+', df$rtng)
df$rtng <- ifelse(df$RATING == 'B2', 'B', df$rtng)
df$rtng <- ifelse(df$RATING == 'B3', 'B-', df$rtng)
df$rtng <- ifelse(df$RATING == 'Caa1', 'CCC+', df$rtng)
df$rtng <- ifelse(df$RATING == 'Caa2', 'CCC', df$rtng)
df$rtng <- ifelse(df$RATING == 'Caa3', 'CCC-', df$rtng)
df$rtng <- ifelse(df$RATING == 'Ca', 'CC', df$rtng)
df$rtng <- ifelse(is.na(df$rtng), df$RATING, df$rtng)
df$ttm <- difftime(df$MATURITY.x , df$trd_exctn_dt, units = 'days')

write_csv(df, 'data_half2.csv')
rm(trace)
rm(trace2)
rm(trace3)
rm(trace4)
rm(trace5)
rm(trace7)
rm(trace8)
rm(trace9)
rm(trace10)
rm(trace11)
