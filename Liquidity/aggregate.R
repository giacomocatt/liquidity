library("readr")
library("tidyr")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

df_full <- read.csv('data_half.csv')
df_full$ttm_agg <- ceiling(df_full$ttm/30)
df_raw <- df_full[df_full$rtng %in% c('AAA', 'AA', 'A', 'BBB'), ]
df_raw$trd_exctn_dt <- as.Date(df_raw$trd_exctn_dt)
rmoutliers <- function(y){
  Q <- quantile(y, probs=c(.005, .995), na.rm = T)
  x<-y
  x[(y > Q[2] & y < Q[1])]<- NA
  x
}

df <- df_raw %>%
  group_by(rtng, rpt_side_cd, cntra_mp_id, ttm_agg) %>%
  mutate(price =rmoutliers(rptd_pr))

df1 <- df[!is.na(df$price),]

#Aggregate

df_daily <- df %>%
  group_by(trd_exctn_dt, rtng, rpt_side_cd, cntra_mp_id, ttm_agg) %>%
  summarize(mean_price = weighted.mean(rptd_pr, entrd_vol_qt), mean_vol = mean(entrd_vol_qt)/1000, mean_yield = weighted.mean(yld_pt, entrd_vol_qt),
            mean_off_yield = mean(OFFERING_YIELD), mean_off_pr = mean(OFFERING_PRICE))

df_daily <- read.csv('daily.csv')
df_daily$trd_exctn_dt <- as.Date(df_daily$trd_exctn_dt)
df_daily$mat <- cut(df_daily$ttm_agg, breaks = c(0,1,2,5,10,200), labels=c('<1', '(1,2]', '(2,5]', '(5,10]', '>10'))
rmoutliers <- function(y){
  Q <- quantile(y, probs=c(.005, .995), na.rm = T)
  x<-y
  x[(y > Q[2] & y < Q[1])]<- NA
  x
}
price <- df_daily[,c(1,2,3,4,5,6,11)]
price_c <- price[price$cntra_mp_id == 'C', ]
price_d <- price[price$cntra_mp_id == 'D', ]
price_c <- spread(price_c, rpt_side_cd, mean_price)
price_c <- price_c[,-c(3)]
price_d <- price_d %>%
  group_by(trd_exctn_dt, rtng, ttm_agg, mat) %>%
  mutate(mid = mean(mean_price))
price_d <- price_d[,-c(3,4,6)]
price_d <- price_d[!duplicated(price_d),]
price_new <- merge(price_c, price_d, by=c('trd_exctn_dt', 'rtng', 'ttm_agg', 'mat'))
price_new$b_a <- (price_new$S - price_new$B)/price_new$mid*100


prc <- price_new %>%
  group_by(rtng, ttm_agg, mat) %>%
  mutate(b_a =rmoutliers(b_a))

smoot_ba <- prc %>%
  group_by(rtng, mat) %>%
  mutate(smoothed_ba = rollmean(b_a, k=120, fill=NA, align='right', na.rm=T))
smoot_ba <- smoot_ba[!is.na(smoot_ba$mat), ]
#smoot_ba <- smoot_ba %>%
#  group_by(trd_exctn_dt, rtng, mat) %>%
#  summarize(avg_ba = mean(smoothed_ba))

ggplot(data = subset(smoot_ba, smoot_ba$mat %in% c('<1', '(1,2]', '(2,5]')), aes(x = trd_exctn_dt, y = smoothed_ba, color=mat)) +
  geom_line()+  facet_wrap(~rtng, scales = "free")
#AAA from 2015-06 -> Fed tightening, BBB from May 2013 -> Fed stops buying Treasuries?

vol <- df_daily %>%
  group_by(rtng, mat) %>%
  mutate(smoothed_vol = rollmean(mean_vol, k=120, fill=NA, align='right', na.rm=T))
vol <- vol %>%
  group_by(trd_exctn_dt, rtng, cntra_mp_id, mat) %>%
  summarize(avg_vol = mean(smoothed_vol))
ggplot(data = subset(vol, vol$mat %in% c('<1', '(1,2]', '(2,5]')), aes(x = trd_exctn_dt, y = avg_vol, color=mat)) +
  geom_line()+  facet_wrap(~rtng, scales = "free")

yield <- df_daily %>%
  group_by(rtng, mat) %>%
  mutate(smoothed_yield = rollmean(mean_yield, k=120, fill=NA, align='right', na.rm=T))
yield <- yield %>%
  group_by(trd_exctn_dt, rtng, cntra_mp_id, mat) %>%
  summarize(avg_yield = mean(smoothed_yield))
ggplot(data = subset(yield,yield$mat %in% c('<1', '(1,2]', '(2,5]')), aes(x = trd_exctn_dt, y = smoothed_yield, color=mat)) +
  geom_line()+  facet_wrap(~rtng, scales = "free")
