
library("readr")
library("tidyr")
library(dplyr)
library(ggplot2)
setwd("C:/Users/giaco/Desktop/phd/Current Projects/Liquidity/Data")

df_issuers=read_csv(file='Bond_issue.csv')
df_rating=read_csv(file='Bond_rating.csv')

#merge issuance with rating

df_rating <- df_rating[!(df_rating$RATING == "NR"),]
df_rating <- df_rating[!duplicated(df_rating$ISSUE_ID),]

df_origin = merge(df_issuers, df_rating, by = 'ISSUE_ID', all.x = TRUE)

keep <- c("ISSUE_ID"                       ,"ISSUER_ID.x"                    ,"PROSPECTUS_ISSUER_NAME.x"      ,
          "ISSUER_CUSIP.x"                 ,"ISSUE_CUSIP.x"                  ,"ISSUE_NAME.x"                  ,
          "MATURITY.x"                     ,"SECURITY_LEVEL"                 ,"SECURITY_PLEDGE"               ,
           "ENHANCEMENT"                   , "COUPON_TYPE"                   , "CONVERTIBLE"                   ,
           "MTN"                           , "ASSET_BACKED"                  , "YANKEE"                        ,
           "CANADIAN"                      , "OID"                           , "FOREIGN_CURRENCY"              ,
           "SLOB"                          , "ISSUE_OFFERED_GLOBAL"          , "SETTLEMENT_TYPE"               ,
           "GROSS_SPREAD"                  , "SELLING_CONCESSION"            , "REALLOWANCE"                   ,
           "COMP_NEG_EXCH_DEAL"            , "RULE_415_REG"                  , "SEC_REG_TYPE1"                 ,
           "SEC_REG_TYPE2"                 , "RULE_144A"                     , "TREASURY_SPREAD"               ,
           "TREASURY_MATURITY"             , "OFFERING_AMT"                  , "OFFERING_DATE.x"               ,
           "OFFERING_PRICE"                , "OFFERING_YIELD"                , "DELIVERY_DATE"                 ,
           "UNIT_DEAL"                     , "FORM_OF_OWN"                   , "DENOMINATION"                  ,
           "PRINCIPAL_AMT"                 , "COVENANTS"                     , "DEFEASED"                      ,
           "DEFEASANCE_TYPE"               , "DEFEASED_DATE"                 , "DEFAULTED"                     ,
           "TENDER_EXCH_OFFER"             , "REDEEMABLE"                    , "REFUND_PROTECTION"             ,
           "REFUNDING_DATE"                , "PUTABLE"                       , "OVERALLOTMENT_OPT"             ,
           "ANNOUNCED_CALL"                , "ACTIVE_ISSUE"                  , "DEP_ELIGIBILITY"               ,
           "PRIVATE_PLACEMENT"             , "BOND_TYPE"                     , "SUBSEQUENT_DATA"               ,
           "PRESS_RELEASE"                 , "ISIN"                          , "PERPETUAL"                     ,
           "SEDOL"                         , "EXCHANGEABLE"                  , "FUNGIBLE"                      ,
           "REGISTRATION_RIGHTS"           , "PREFERRED_SECURITY"            , "COMPLETE_CUSIP.x"              ,
           "ACTION_TYPE"                   , "EFFECTIVE_DATE"                , "ACTION_PRICE"                  ,
           "ACTION_AMOUNT"                 , "AMOUNT_OUTSTANDING"            , "NEGATIVE_PLEDGE_COVENANT"      ,
           "COVENANT_DEFEAS_WO_TAX_CONSEQ" , "LEGAL_DEFEASANCE"              , "DEFEASANCE_WO_TAX_CONSEQ"      ,
           "CROSS_DEFAULT"                 , "CROSS_ACCELERATION"            , "CHANGE_CONTROL_PUT_PROVISIONS" ,
           "VOTING_POWER_PERCENTAGE"       , "VOTING_POWER_PERCENTAGE_ERP"   , "RATING_DECLINE_TRIGGER_PUT"    ,
           "RATING_DECLINE_PROVISION"      , "DECLINING_NET_WORTH"           , "DECLINING_NET_WORTH_TRIGGER"   ,
           "DECLINING_NET_WORTH_PERCENTAGE", "DECLINING_NET_WORTH_PROVISIONS", "AFTER_ACQUIRED_PROPERTY_CLAUSE",
           "ECONOMIC_COV_DEF"              , "ASSET_SALE_CLAUSE"             , "FIX_FREQUENCY"                 ,
           "DETERMINATION_DATE"            , "GREATER_OF"                    , "LESSER_OF"                     ,
           "SEE_NOTE"                      , "RESET_DATE"                    , "DETERMINATION_DATE_ORIG"       ,
           "RESET_DATE_ORIG"               , "CONV_COMMOD_CUSIP"             , "CONV_COMMOD_ISSUER"            ,
           "CONV_COMMOD_TYPE"              , "EXCHANGE"                      , "TICKER"                        ,
           "CONV_PRICE"                    , "QTY_OF_COMMOD"                 , "PERCENT_OF_OUTSTANDING_COMMOD" ,
           "CONV_CASH"                     , "CONV_EFF_DATE"                 , "CONV_EXP_DATE"                 ,
           "DILUTION_PROTECTION"           , "COMMOD_PRICE"                  , "CONV_PREMIUM"                  ,
           "CONV_REDEMP_EXCEPTION"         , "CONV_REDEMP_DATE"              , "CONV_PRICE_PERCENT"            ,
           "CONV_PART_TRADE_DAYS"          , "CONV_TOTAL_TRADE_DAYS"         , "CONV_PERIOD_SPEC"              ,
           "CONV_PERIOD_DAYS"              , "AGENT_ID"                      , "SHARES_OUTSTANDING"            ,
           "ORIG_CONV_PRICE"               , "ORIG_COMMOD_PRICE"             , "ORIG_CONV_PREMIUM"             ,
           "ORIG_SHARES_OUTSTANDING"       , "ORIG_PERCENT_OUTSTANDING_COM"  , "ORIG_QTY_OF_COMMOD"            ,
           "AS_OF_DATE"                    , "REASON.x"                      , "CHANGE_DATE"                   ,
           "SPLIT_DATE"                    , "SPLIT_RATIO"                   , "CONDITIONAL_CONV_TERMS"        ,
           "SOFT_CALL_MAKE_WHOLE"          , "PEPS"                          , "PERCS"                         ,
           "CONV_PROHIBITED_FROM"          , "CONVERT_ON_CALL"               , "COCO_START_DATE"               ,
           "COCO_END_DATE"                 , "COCO_INITIAL_TRIGGER_PERCENT"  , "COCO_TRIGGER_EXPRESSED_AS"     ,
           "COCO_CHANGE_RATE"              , "COCO_MIN_TRIGGER_LEVEL"        , "COCO_CHANGE_FREQUENCY"         ,
           "COCO_TRADE_DAYS"               , "COCO_TRADE_DAYS_IN_PREVIOUS"   , "SC_MAKE_WHOLE_START_DATE"      ,
           "SC_MAKE_WHOLE_END_DATE"        , "SC_MAKE_WHOLE_DECREMENT_TYPE"  , "SC_MAKE_WHOLE_INITIAL_AMOUNT"  ,
           "SC_MAKE_WHOLE_CHANGE_PERCENT"  , "PEPS_MAX_CONVERSION_RATIO"     , "PEPS_MIN_CONVERSION_RATIO"     ,
           "PEPS_HIGHER_PRICE"             , "PEPS_LOWER_PRICE"              , "PEPS_ISSUE_PRICE"              ,
           "PERCS_MAX_PAYOFF"              , "DATED_DATE"                    , "FIRST_INTEREST_DATE"           ,
           "INTEREST_FREQUENCY"            , "COUPON"                        , "PAY_IN_KIND"                   ,
           "PAY_IN_KIND_EXP_DATE"          , "COUPON_CHANGE_INDICATOR"       , "DAY_COUNT_BASIS"               ,
           "LAST_INTEREST_DATE"            , "CURRENCY"                      , "AMT_OFFERED"                   ,
           "CONVERSION_RATE"               , "CONSOLIDATION_MERGER"          , "DIVIDENDS_RELATED_PAYMENTS_IS" ,
           "FUNDED_DEBT_IS"                , "INDEBTEDNESS_IS"               , "INVESTMENTS"                   ,
           "LIENS_IS"                      , "MAINTENANCE_NET_WORTH"         , "RESTRICTED_PAYMENTS"           ,
           "SALES_LEASEBACK_IS"            , "SALE_ASSETS"                   , "SENIOR_DEBT_ISSUANCE"          ,
           "STOCK_ISSUANCE_ISSUER"         , "STOCK_TRANSFER_SALE_DISP"      , "SUBORDINATED_DEBT_ISSUANCE"    ,
           "TRANSACTION_AFFILIATES"        , "NET_EARNINGS_TEST_ISSUANCE"    , "FIXED_CHARGE_COVERAGE_IS"      ,
           "LEVERAGE_TEST_IS"              , "ISSUER_ID_AFFECTED"            , "FILING_DATE"                   ,
           "SETTLEMENT"                    , "OTHER_SEC_TYPE"                , "OTHER_SEC_ISSUER"              ,
           "SEC_CUSIP"                     , "QUANTITY"                      , "DATE_TRANSFERABLE"             ,
           "DATE_SUBJ_ADJUSTMENT"          , "MARKET_PRICE"                  , "ALLOCATED_OFFERING_PRICE_OTHER",
           "OVERALLOTMENT_EXPIRATION_DATE" , "EXERCISED"                     , "EXERCISED_DATE"                ,
           "AMOUNT"                        , "NOTIFICATION_PERIOD"           , "NEXT_PUT_DATE"                 ,
           "NEXT_PUT_PRICE"                , "BORROWING_RESTRICTED"          , "DIVIDENDS_RELATED_PAYMENTS_SUB",
           "FUNDED_DEBT_SUB"               , "INDEBTEDNESS_SUB"              , "STOCK_ISSUANCE"                ,
           "PREFERRED_STOCK_ISSUANCE"      , "INVESTMENTS_UNRESTRICTED_SUBS" , "SALE_XFER_ASSETS_UNRESTRICTED" ,
           "SUBSIDIARY_REDESIGNATION"      , "SUBSIDIARY_GUARANTEE"          , "SALES_LEASEBACK_SUB"           ,
           "LIENS_SUB"                     , "FIXED_CHARGE_COVERAGE_SUB"     , "LEVERAGE_TEST_SUB"             ,
           "UNIT_CUSIP"                    , "TOTAL_UNITS_OFFERED"           , "PRINCIPAL_AMT_PER_UNIT"        ,
           "ALLOCATED_OFFERING_PRICE_UNIT" , "CUSIP_NAME"                    , "INDUSTRY_GROUP"                ,
           "INDUSTRY_CODE"                 , "ESOP"                          , "IN_BANKRUPTCY"                 ,
           "PARENT_ID"                     , "NAICS_CODE"                    , "COUNTRY_DOMICILE"              ,
           "SIC_CODE"                      , "RATING_TYPE"                   , "RATING_DATE"                   ,
           "RATING"                        , "RATING_STATUS"                 , "REASON.y"                      ,
           "RATING_STATUS_DATE"            , "INVESTMENT_GRADE")
df_origin<- df_origin[, names(df_origin) %in% keep]
df_origin <- df_origin[yankee == "N" & is.na(yankee) & # no foreign issuer
                       canadian == "N" & is.na(canadian)& # not Canadian
                       foreign_currency == "N" & bond_type %in% c(
                         "CDEB", # US Corporate Debentures
                         "CMTN", # US Corporate MTN (Medium Term Note)
                         "CMTZ", # US Corporate MTN Zero
                         "CZ", # US Corporate Zero,
                         "USBN" # US Corporate Bank Note
                       ),]

df_origin_x <- filter(df_origin, SLOB == "N" | is.na(SLOB), # secured lease obligation
                      is.na(SECURITY_PLEDGE), # unsecured bonds
                      ASSET_BACKED == "N" | is.na(ASSET_BACKED), # not asset backed
                      DEFEASED == "N" | is.na(DEFEASED), # not defeased
                      is.na(DEFEASED_DATE),
                      BOND_TYPE %in% c(
                        "CDEB", # US Corporate Debentures
                        "CMTN", # US Corporate MTN (Medium Term Note)
                        "CMTZ", # US Corporate MTN Zero
                        "CZ", # US Corporate Zero,
                        "USBN" # US Corporate Bank Note
                      ), 
                      PAY_IN_KIND != "Y" | is.na(PAY_IN_KIND), # not payable in kind
                      is.na(PAY_IN_KIND_EXP_DATE),
                      YANKEE == "N" | is.na(YANKEE), # no foreign issuer
                      CANADIAN == "N" | is.na(CANADIAN), # not Canadian
                      FOREIGN_CURRENCY == "N", # USD
                      COUPON_TYPE %in% c(
                        "F", # fixed coupon
                        "Z" # zero coupon
                      ), 
                      is.na(FIX_FREQUENCY),
                      COUPON_CHANGE_INDICATOR == "N",
                      INTEREST_FREQUENCY %in% c(
                        "0", # per year
                        "1",
                        "2",
                        "4",
                        "12"
                      ),
                      RULE_144A == "N", # publicly traded
                      PRIVATE_PLACEMENT == "N" | is.na(PRIVATE_PLACEMENT),
                      DEFAULTED == "N", # not defaulted
                      is.na(FILING_DATE),
                      is.na(SETTLEMENT),
                      CONVERTIBLE == "N", # not convertible
                      is.na(EXCHANGE),
                      PUTABLE == "N" | is.na(PUTABLE), # not putable
                      UNIT_DEAL == "N" | is.na(UNIT_DEAL), # not issued with another security
                      EXCHANGEABLE == "N" | is.na(EXCHANGEABLE), # not exchangeable
                      PERPETUAL == "N", # not perpetual
                      PREFERRED_SECURITY == "N" | is.na(PREFERRED_SECURITY))


