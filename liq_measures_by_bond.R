library("readr")
library("tidyr")
library(dplyr)
library(tidyverse)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

#By bond

df <- read.csv('data_half_complete+19_22.csv')
df_origin <- read.csv('Issue_&_rating.csv')

additions <- df_origin[, names(df_origin) %in% c('OFFERING_AMT', "AMOUNT_OUTSTANDING", "COMPLETE_CUSIP.x", 'PRINCIPAL_AMT', 'OFFERING_PRICE')]
data <- merge(df, additions, by.x = 'cusip_id', by.y='COMPLETE_CUSIP.x', all.x=T)
data <- data[!is.na(data$trd_exctn_dt),]
data$ttm_agg <- ceiling(data$ttm/30)

daily <- data %>%
  group_by(trd_exctn_dt, cusip_id, rpt_side_cd, cntra_mp_id, ttm_agg) %>%
  summarize(price = weighted.mean(rptd_pr, entrd_vol_qt), vol = mean(entrd_vol_qt), yield = weighted.mean(yld_pt, entrd_vol_qt))

price_c <- daily[daily$cntra_mp_id == 'C', ]
price_d <- daily[daily$cntra_mp_id == 'D', ]
price_c <- spread(price_c, rpt_side_cd, price)
price_c <- price_c[,-c(3)]
price_d <- price_d %>%
  group_by(trd_exctn_dt, cusip_id, ttm_agg) %>%
  mutate(mid = mean(price))
price_d <- price_d[,-c(3,4,6)]
price_d <- price_d[!duplicated(price_d),]
price_new <- merge(price_c, price_d, by=c('cusip_id', 'trd_exctn_dt', 'ttm_agg'), all=T)
price_new <- price_new %>% 
  group_by(trd_exctn_dt, cusip_id, ttm_agg) %>%
  summarize(S = mean(S, na.rm=T), B = mean(B, na.rm=T), mid = mean(mid, na.rm=T))
price_new$b_a <- (price_new$S - price_new$B)/price_new$mid*100

data$outstanding <- data$OFFERING_AMT*10^3 + data$AMOUNT_OUTSTANDING
data$volume <- data$entrd_vol_qt * data$rptd_pr

turnover_daily <- data %>%
  group_by(trd_exctn_dt, cusip_id, rpt_side_cd, ttm_agg) %>%
  summarize( volume = mean(entrd_vol_qt, na.rm=T))
turnover_disp <- spread(turnover_daily, rpt_side_cd, volume)
turnover_disp$v_diff <- turnover_disp$S - turnover_disp$B
turnover_disp$v_abs_diff <- abs(turnover_disp$S - turnover_disp$B)
turnover_disp$v_rel_diff <- 200*abs(turnover_disp$S - turnover_disp$B)/(turnover_disp$S + turnover_disp$B)
turnover_daily <- turnover_daily  %>%
  group_by(trd_exctn_dt, cusip_id, ttm_agg) %>%
  summarize(turnover1 = mean(turnover1, na.rm=T), outstanding = mean(outstanding, na.rm=T))
turnover_daily <- merge(turnover_daily, turnover_disp, by=c('trd_exctn_dt', 'cusip_id', 'ttm_agg'))

df <- merge(price_new, turnover_daily, by = c('cusip_id', 'trd_exctn_dt', 'ttm_agg'), all=T)
yield <- daily[,names(daily) %in% c('trd_exctn_dt', 'cusip_id', 'ttm_agg', 'yield')]
yield <- yield %>% 
  group_by(trd_exctn_dt, cusip_id, ttm_agg) %>% 
  summarise(yield = mean(yield, na.rm=T))
df <- merge(df, yield , by = c('trd_exctn_dt', 'cusip_id', 'ttm_agg'), all=T)

daily_impact <- data %>%
  group_by(trd_exctn_dt, cusip_id, ttm_agg) %>%
  summarize(p_i = 100*mean(abs(rptd_pr - lag(rptd_pr))/lag(rptd_pr)/volume, na.rm=T), tunrover = 100*mean(volume/outstanding, na.rm=T))