library("readr")
library("tidyr")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plm)
library(ivreg)
library(broom)
library(latex2exp)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

df <- read_csv('liq_cs_firmspec_tot.csv')
yc <- read_csv('yield_curve.csv')
yc <- yc[, names(yc) %in% c('Date', '1 Mo', '3 Mo', '10 Yr')]
names(yc) <- c('Date', 'short_r', 'short', 'long')
yc$slope <- yc$long - yc$short
yc <- yc[,names(yc) %in% c('Date', 'short_r', 'slope')]
yc$Date <- as.Date(yc$Date, '%m/%d/%y')
df <- full_join(df, yc, by = c('trd_exctn_dt' = 'Date'))
df <- df[!is.na(df$cusip_id),]
library(zoo)
df <- df %>%
  group_by(cusip_id) %>%
  mutate(S_ma = rollmean(S, k = 5, fill =NA, na.rm=T), B_ma = rollmean(B, k = 5, fill =NA, na.rm=T))
df$BA <- 200*(df$S_ma - df$B_ma)/(df$S_ma + df$B_ma)
df$rtng <- df$RATING
# Ouliers

quartiles <- c(quantile(df$cs, probs=c(.25, .75), na.rm = T), quantile(df$BA, probs=c(.25, .75), na.rm = T), quantile(df$turnover1, probs=c(.25, .75), na.rm = T))
IQR <- c(IQR(df$cs, na.rm=T), IQR(df$BA, na.rm=T), IQR(df$turnover1, na.rm=T))

df <- subset(df, df$cs > quartiles[1] - 1.5*IQR[1] & df$cs < quartiles[2] + 1.5*IQR[1] &
                df$BA > quartiles[3] - 1.5*IQR[2] & df$BA < quartiles[4] + 1.5*IQR[2] &
               df$turnover1 > quartiles[5] - 1.5*IQR[3] & df$turnover1 < quartiles[6] + 1.5*IQR[3])

#Infrequent trading

fr <- df %>% 
  group_by(cusip_id, quarter, year) %>% 
  summarise(fr = n())
fr <- fr[fr$fr>12, ]
df <- full_join(df, fr)
df <- df[!is.na(df$fr),]

#Create IVs

library(plyr)
iv <- ddply(df, .(quarter,year, rtng), summarise,
                     iv_ba = mean(b_a, na.rm=TRUE),
                     iv_pi = mean(p_i, na.rm=TRUE),
                     iv_turnover = mean(tunrover, na.rm=TRUE),
                     iv_BA = mean(BA, na.rm=TRUE),
                     iv_turn1 = mean(turnover1, na.rm=TRUE),
                     iv_turn2 = mean(turnover2, na.rm=TRUE))
detach(package:plyr)
iv$month <- ifelse(iv$quarter=='Q1', '01', ifelse(iv$quarter=='Q2', '04', ifelse(iv$quarter=='Q3', '07', ifelse(iv$quarter=='Q4', '10', 'NA'))))
iv$date <- with(iv,paste(year,month,'01', sep="-"))
iv$date <- as.Date(iv$date,format="%Y-%m-%d")
library(xts)
ivs <- split(iv, iv$rtng)
lags <- sapply(ivs, 
               function(x) {
                 x <- as.data.frame(x)
                 x <- xts(x[,-12], order.by = as.Date(x$date))
                 x$iv_turn1_lag = lag(x$iv_turn1)
                 x$iv_BA_lag = lag(x$iv_BA)
                 x$iv_turnover_lag = lag(x$iv_turnover)
                 x})
lagss <- lapply(lags, 
                function(x){
                  y = data.frame(date =index(x), coredata(x))
                  y})
lagss<-list_rbind(lagss)
iv <- lagss[,-1]
iv$year <- as.double(iv$year)
df <- full_join(df, iv)
df <- df[!is.na(df$rtng),]

#df$rat <- ifelse(df$rtng == 'AAA+', 'AAA+', NA)
#df$rat <- ifelse(df$rtng %in% c('AAA', 'AAA-', 'AA+', 'AA-', 'AA', 'A+', 'A', 'A-', 'Aa3', 'A2',
#                                'A3', 'Aa2', 'Aa1'), 'A<<AAA', df$rat)
#df$rat <- ifelse(df$rtng %in% c('BBB+', 'BBB-', 'BBB', 'BB-', 'BB', 'BB+', 'B+', 'B', 'B-',
#                                'Ba3', 'Baa1', 'Baa3', 'Baa2', 'Ba2', 'Ba1', 'B2'), 'B<<BBB+', df$rat)
#df$rat <- ifelse(df$rtng %in% c('CCC+', 'Caa1', 'Caa3', 'Caa2', 'CCC', 'CC', 'CC+', 'C+', 'C', 'D'),
#                 'Junk', df$rat)


df <- read_csv('data_regr_full.csv')
df <- df[!duplicated(df[,c('trd_exctn_dt', 'cusip_id')]),]
vars <- c('trd_exctn_dt','quarter', 'year', 'cs', 'turnover1', 'rat', 'short_r', 'slope', 'opmad', 'debt_at' ,'cash_debt', 'debt_capital', 
          'iv_turn1_lag', 'rtng', 'short_r', 'opmad_lag', 'debt_at_lag', 'cash_debt_lag', 'debt_capital_lag', 'ttm_agg')
df <- drop_na(df[, names(df) %in% vars])
data_quarterly <- split(df, list(df$quarter, df$year))

regr <- function(x) {
  model <- ivreg(cs ~ turnover1  + opmad + debt_at + cash_debt + debt_capital + short_r + slope + rtng + ttm_agg|  +
          iv_turn1_lag + opmad_lag + debt_at_lag + cash_debt_lag + debt_capital_lag+ short_r 
        + slope + rtng + ttm_agg,
        data =x)
  model_tidy <- tidy(model, conf.int=T, conf.level=.95)
  estim <- c(model_tidy$conf.low[2], model_tidy$estimate[2], model_tidy$conf.high[2])
  estim
}
data_quarterly <- data_quarterly[-c(1,57)]
pooled_model <- sapply(data_quarterly, regr)
pooled_model_df <- t(pooled_model)
pooled_model_df <- as.data.frame(pooled_model_df)
pooled_model_df$quarter <- row.names(pooled_model_df)
aux <- str_split(pooled_model_df$quarter, "20")
for (i in 1:66) {
  pooled_model_df$qq[i] <- aux[[i]][1]
  pooled_model_df$year[i] <- aux[[i]][2]
}
pooled_model_df$year[pooled_model_df$year==''] <- '20'
pooled_model_df$qq <- as.character(pooled_model_df$qq)
pooled_model_df$year <- as.double(pooled_model_df$year)
pooled_model_df <- pooled_model_df[order(pooled_model_df[,6],pooled_model_df[,5]),]

coeffs_head = head(pooled_model_df, 30)
coeffs_tail = tail(pooled_model_df, 30)

polished <- pooled_model_df[!(pooled_model_df$V1 < -100) & !(pooled_model_df$V3 > 100),]

ggplot(
  pooled_model_df
#  coeffs_tail
#  polished
   ) + 
  geom_point(aes(fct_inorder(quarter), V2)) + 
  geom_errorbar(aes(x = quarter, ymin = V1, ymax = V3)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', col='red') +
  xlab(TeX('Quarter')) + ylab(TeX('$\\beta$')) + coord_cartesian(ylim=c(-25,150))  +
  theme(axis.text.x = element_text(size = 7, angle=45), axis.text.y = element_text(size=7))

df1 <- df1[,-c(1,2,6)]
df <- df[, -1]
df <- df[, names(df) !='RATING']
df <- df[, c("trd_exctn_dt" ,    "cusip_id"    ,     "ttm_agg"  ,        "quarter"   ,       "year"       ,      "ISSUER_CUSIP.x" ,  "S"            ,    "B"      ,         
         "mid"              ,"b_a"             , "p_i"          ,    "tunrover"      ,   "yield"          ,  "rf"             ,  "cs"               ,"date"       ,     
          "cusip"           , "opmad"          ,  "debt_at"     ,     "cash_debt"    ,    "debt_capital"  ,   "debt_at_lag"   ,   "debt_capital_lag", "opmad_lag" ,      
          "cash_debt_lag"   , "short_r"        ,  "slope"       ,     "S_ma"         ,    "B_ma"          ,   "BA"            ,   "rtng"            , "turnover1" ,      
          "turnover2"       , "iv_ba"          ,  "iv_pi"       ,     "iv_turnover"  ,    "iv_BA"         ,   "iv_turn1"      ,   "iv_turn2"        , "month"     ,      
          "date.1"          , "iv_turn1_lag"   ,  "iv_BA_lag"   ,     "iv_turnover_lag")]
df <- rbind(df1, df)


#MP shocks
library(readxl)
mp_shocks <- read_excel('MP_news_shocks.xlsx', sheet=2)
y <- merge(df, mp_shocks, by.x = 'trd_exctn_dt', by.y = 'fomc')
py <- y[!duplicated(y[,c('trd_exctn_dt','cusip_id')]),]
mp <- plm(cs ~ ff.shock.0 + turnover1+ ff.shock.0 * turnover1 + opmad + debt_at + cash_debt + debt_capital + short_r + slope + rtng + ttm_agg|ff.shock.0  +
            iv_turn1_lag + ff.shock.0*iv_turn1_lag + opmad_lag + debt_at_lag + cash_debt_lag + debt_capital_lag+ short_r +ttm_agg
    + slope + rtng, data=py, index=c('cusip_id', 'trd_exctn_dt'), model ='within', na.rm=T)
summary(mp)
