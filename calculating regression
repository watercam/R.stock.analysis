####################################
### calculating regression #########
### 11/14 lecture ##################
####################################

install.packages("quantmod")
install.packages("xts")
library(quantmod)
library(xts)

load( file = "peer_all_df.Rdata")

load( file = "regression_dataset.Rdata")

## download exchange rate data
usd.eur <- getSymbols("DEXUSEU", src = "FRED", auto.assign = FALSE)
usd.eur <- na.omit(usd.eur)

## calculate daily % change
usd.eur$eur.change <- Delt(usd.eur$DEXUSEU) * 100

## limit to variables of interest
date <- index(usd.eur)
usd.eur.df <- cbind(data.frame(date), data.frame(usd.eur))
regression_data_all <- merge(regression_dataset, usd.eur.df)
regression_data_vars <- regression_data_all[ , c(1,5,6,11,12,14)]

## subset down to estimation and event window
event_study <- subset(regression_data_vars, regression_data_vars$date >=
                      "2014-01-19" & regression_data_vars$date <= "2015-01-20")

## create one subset that only includes estimation window (drop last event)
es <- subset(event_study, event_study$date < "2015-01-20")

## run 1 factor regression; store in market_model
market_model <- lm( es$JNJ.Ret ~ es$GSPC.Ret)
summary(market_model) #prints results of OLS regression

## run 3 factor regression; store in threefactor_model
threefactor_model <- lm(es$JNJ.Ret ~ es$GSPC.Ret + es$EW.Ret + es$eur.change)
summary(threefactor_model) #prints results of OLS regression

## run 3 factor regression (VW instead of EW); store in threefactor_model
threefactor_modelVW <- lm(es$JNJ.Ret ~ es$GSPC.Ret + es$VW.Ret + es$eur.change)
summary(threefactor_modelVW) #prints results of OLS regression

## add predicted returns from both models to event_study 
event_study$JNJ.Ret.Pred.M <- summary(market_model)$coefficients[1] +
  (summary(market_model)$coefficients[2] * event_study$GSPC.Ret)
event_study$JNJ.Ret.Pred.3F <- summary(threefactor_model)$coefficients[1] +
  (summary(threefactor_model)$coefficients[2] * event_study$GSPC.Ret) +
  (summary(threefactor_model)$coefficients[3] * event_study$EW.Ret) +
  (summary(threefactor_model)$coefficients[4] * event_study$eur.change)

## calculate abnormal return (actual - expected)
event_study$JNJ.Ab.M <- event_study$JNJ.Ret - event_study$JNJ.Ret.Pred.M
event_study$JNJ.Ab.3F <- event_study$JNJ.Ret - event_study$JNJ.Ret.Pred.3F

## add t stat to event_study
event_study$tstat.M <- event_study$JNJ.Ab.M / summary(market_model)$sigma
event_study$tstat.3F <- event_study$JNJ.Ab.3F / summary(threefactor_model)$sigma

## what happened on the day of event?
tail(event_study, n = 1)

