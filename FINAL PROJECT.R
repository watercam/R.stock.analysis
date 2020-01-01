###########################################
###  Final Project            #############
###  FIN 4604 Section 2       #############
###  Joseph Sistare 11/27/19  #############
###########################################

Sys.localeconv()
getwd()
install.packages("quantmod")
install.packages("xts")
library(quantmod)
library(xts)



## step 3

cost <- getSymbols("COST", src = "yahoo", auto.assign = FALSE)
gspc <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
wmt <- getSymbols("WMT", src = "yahoo", auto.assign = FALSE)
tgt <- getSymbols("TGT", src = "yahoo", auto.assign = FALSE)
bbby <- getSymbols("BBBY", src = "yahoo", auto.assign = FALSE)



## step 4

jpy.usd.all <- getSymbols("DEXJPUS", src = "FRED", auto.assign = FALSE)
jpy.usd <- subset(jpy.usd.all[ ,1], index(jpy.usd.all) >= "2017-11-27" &
                       index(jpy.usd.all) <= "2019-11-27")

plot( x = index(jpy.usd), xlab = "Date", xaxt = "n",
      y = jpy.usd$DEXJPUS, ylab = "USD/JPY",
      ylim = c(100,120), type = "l",
      col = "blueviolet", lwd = 2, lty = 1,
      main = "Japanese Yen to One U.S. Dollar
November 27, 2017 - November 27, 2019")
axis.Date(1,Day,at=seq(as.Date("2017/11/27"), as.Date("2019/11/27"),by="quarters"), format="%m-%Y")

jpy.usd$jpy.ret <- Delt(jpy.usd$DEXJPUS) * 100

head(jpy.usd)
tail(jpy.usd)

## step 5
## most recent earnings announcement: October 03, 2019
## estimation period: October 2, 2018 - October 2, 2019

cost.sub <- subset(cost[ ,6], index(cost) >= "2018-10-02" &
                    index(cost) <= "2019-10-03")
cost.sub$cost.ret <- Delt(cost.sub$COST.Adjusted) * 100

par(mar=c(5,5,5,5))
plot( x = index(cost.sub), xlab = "", xaxt = "n",
      y = cost.sub$cost.ret, ylab = "", yaxt = "n",
      ylim = c(-10,10), type = "l",
      col = "cornflowerblue", lwd = 1, lty = 1)
axis(4)
mtext(side = 4, line = 3, "Daily Total Return (%)")
abline( h = 0, lty = 2, col = "black")
par (new = TRUE)
par(mar=c(5,5,5,5))
plot( x = index(cost.sub), xlab = "Date",
      y = cost.sub$COST.Adjusted, ylab = "Adjusted Close ($)",
      ylim = c(175,325), type = "l",
      col = "firebrick1", lwd = 3, lty = 1,
      main = "Costco Adjusted Close Price
October 2, 2018 - October 3, 2019")
legend( "topleft", c("adj. close price ($)", "daily change (%)"),
        col = c("firebrick1","cornflowerblue"),
        lwd = c(3, 1), lty = c(1, 1))



## step 6

wmt.sub <- subset(wmt[ , 6], index(wmt) >= "2018-10-02" &
                       index(wmt) <= "2019-10-03")
tgt.sub <- subset(tgt[ , 6], index(tgt) >= "2018-10-02" &
                       index(tgt) <= "2019-10-03")
bbby.sub <- subset(bbby[ , 6], index(bbby) >= "2018-10-02" &
                       index(bbby) <= "2019-10-03")
peer <- merge(wmt.sub, tgt.sub)
peer <- merge(peer, bbby.sub)
peer$wmt.ret <- Delt(peer$WMT.Adjusted) * 100
peer$tgt.ret <- Delt(peer$TGT.Adjusted) * 100
peer$bbby.ret <- Delt(peer$BBBY.Adjusted) * 100
peer$weight <- 1/3
peer$ew.ret <- (peer$weight * peer$wmt.ret) + (peer$weight * peer$tgt.ret) + (peer$weight * peer$bbby.ret)
gspc.sub <- subset(gspc[ , 6], index(gspc) >= "2018-10-02" &
                     index(gspc) <= "2019-10-03")
peer <- merge(peer, gspc.sub)
peer$gspc.ret <- Delt(peer$GSPC.Adjusted) * 100
cost.sub <- subset(cost[ , 6], index(cost) >= "2018-10-02" &
                     index(cost) <= "2019-10-03")
peer <- merge(peer, cost.sub)
peer$cost.ret <- Delt(peer$COST.Adjusted) * 100
head(peer)

regression <- merge(peer$cost.ret, peer$gspc.ret)
regression <- merge(regression, peer$ew.ret)
jpy.regress <- subset(jpy.usd[ , 2], index(jpy.usd) >= "2018-10-03" &
                        index(jpy.usd) <= "2019-10-03")
regression <- merge(regression, jpy.regress)
date <- index(regression)
regression.df <- cbind(data.frame(date),data.frame(regression))
regression.df <- na.omit(regression.df)

head(regression)
head(regression.df)

event.study <- regression.df
estimation.window <- subset(event.study, date < "2019-10-03")

tail(estimation.window)

## MARKET MODEL REGRESSION

market.model <- lm(estimation.window$cost.ret ~ estimation.window$gspc.ret)
summary(market.model)

## E(ret on cost) = .06216 + (.84184 * mkt)
## p-value: <2e-16 (99.9% significance)
## residual standrd error: 1.107
## r-squared: .3751

## THREE FACTOR REGRESSION

threefactor.model <- lm(estimation.window$cost.ret ~ estimation.window$gspc.ret 
                        + estimation.window$ew.ret + estimation.window$jpy.ret)
summary(threefactor.model)

## E(ret on cost) = .04956 + (.83406 * mkt) + (.10785 * industry) + (-.49476 * jpy)
## p-value: gspc <2e-16 (99.9% significance)
## p-value ew .0369 (95% significance)
## p-value jpy .0273 (95% significance)
## residual standrd error: 1.092
## r-squared: .3978

event.study$cost.retM <- summary(market.model)$coefficients[1] +
  (summary(market.model)$coefficients[2] * event.study$gspc.ret)
event.study$cost.ret3F <- summary(threefactor.model)$coefficients[1] +
  (summary(threefactor.model)$coefficients[2] * event.study$gspc.ret) +
  (summary(threefactor.model)$coefficients[3] * event.study$ew.ret) +
  (summary(threefactor.model)$coefficients[4] * event.study$jpy.ret)

event.study$cost.abM <- event.study$cost.ret - event.study$cost.retM
event.study$cost.ab3F <- event.study$cost.ret - event.study$cost.ret3F

event.study$tstat.M <- event.study$cost.abM / summary(market.model)$sigma
event.study$tstat.3F <- event.study$cost.ab3F / summary(threefactor.model)$sigma

tail(event.study, n = 1)

## t stat for both models is less than 1.96, therefore,
## the abnormal return of costco on the event window is not statistically different than 0
## costco earned 0.6311% higher than the expected return of the market model
## costco earned 0.2896% higher than the expected return of the three factor model

plot( x = event.study$cost.ret, xlab = "Actual Return (%)",
      y = event.study$cost.retM, ylab = "Predicted Return (%)", ylim = c(-3,3),
      type = "p", col = "red", main = "Costco (COST) Market Model
Actual vs. Predicted Return
October 03,2018 - October 03, 2019")

plot( x = event.study$cost.ret, xlab = "Actual Return (%)",
      y = event.study$cost.ret3F, ylab = "Predicted Return (%)", ylim = c(-3,3),
      type = "p", col = "blue", main = "Costco (COST) Three-Factor Model
Actual vs. Predicted Return
October 03,2018 - October 03, 2019")

## abnormal return with t-stat

plot( x = event.study$date, xlab = "date",
      y = event.study$cost.abM, ylab = "Abnormal Return (%)", ylim = c(-7,5),
      type = "p", col = "red", main = "Costco (COST) Market Model
Abnormal Returns & t-Stat
October 03,2018 - October 03, 2019")
abline( h = 0, lty = 1, col = "black")
lines( x = event.study$date, y = event.study$tstat.M)
abline( h = 1.96, lty = 1, col = "green")
abline( h = -1.96, lty = 1, col = "green")
legend( "bottomright", c("ab. return", "t-stat"),
        col = c("red","black"),
        lwd = c(1, 1), lty = c(1, 1))

plot( x = event.study$date, xlab = "date",
      y = event.study$cost.ab3F, ylab = "Abnormal Return (%)", ylim = c(-7,5),
      type = "p", col = "blue", main = "Costco (COST) Three Factor Model
Abnormal Return & t-Stat
October 03,2018 - October 03, 2019")
abline( h = 0, lty = 1, col = "black")
lines( x = event.study$date, y = event.study$tstat.3F)
abline( h = 1.96, lty = 1, col = "green")
abline( h = -1.96, lty = 1, col = "green")
legend( "bottomright", c("ab. return", "t-stat"),
        col = c("blue","black"),
        lwd = c(1, 1), lty = c(1, 1))

## MARKET MODEL: 10 abnormal return values statistically different than 0
## THREE FACTOR MODEL: 7 abnormal return values statistically different than 0

## MARKET MODEL: .6311% abnormal return on event window
## THREE FACTOR MODEL: .2896% abnormal return on event window

plot( x = event.study$date, xlab = "date",
      y = event.study$cost.abM, ylab = "Abnormal Return (%)",
      type = "p", col = "red", main = "Costco (COST)
Abnormal Returns of Market Model and Three Factor Model
October 03,2018 - October 03, 2019")
abline( h = 0, lty = 1, col = "black")
lines( x = event.study$date, y = event.study$cost.ab3F, type = "p", col = "blue")
abline( h = 1.96, lty = 1, col = "green")
abline( h = -1.96, lty = 1, col = "green")
legend( "bottomright", c("Market Model", "3 Factor Model"),
        col = c("red","blue"),
        lwd = c(1, 1), lty = c(1, 1))

########################################
## written assignment Q2 material ######
########################################

cost.jpy <- subset(cost[ ,6], index(cost) >= "2017-11-27" &
                     index(cost) <= "2019-11-27")
cost.currency <- na.locf(merge(cost.jpy, jpy.usd))
cost.currency <- na.omit(cost.currency)
dates <- index(cost.currency)
cost.currency.df <- cbind(data.frame(dates), data.frame(cost.currency))
cost.currency.df$JPY.Norm <- cost.currency.df$DEXJPUS / 
  cost.currency.df$DEXJPUS[1]
cost.currency.df$COST.Norm <- cost.currency.df$COST.Adjusted / 
  cost.currency.df$COST.Adjusted[1]

plot( x = dates, xlab = "Date", xaxt = "n",
      y = cost.currency.df$COST.Norm, ylab = "Normalized Return (%)",
      ylim = c(.95,1.8), type = "l",
      col = "blueviolet", lwd = 1, lty = 1,
      main = "Costco (COST) & Japanese Yen (USD/JPY)
Normalized Return from Adjusted Close
November 27, 2017 - November 27, 2019")
axis.Date(1,Day,at=seq(as.Date("2017/11/27"), as.Date("2019/11/27"),by="quarters"), format="%m-%Y")
lines( x = dates, y = cost.currency.df$JPY.Norm, lwd = 1, lty = 1,
       col = "forestgreen")
abline( h = 1, lty = 3, col = "black")
legend( "topleft", c("COST", "USD/JPY"),
        col = c("blueviolet","forestgreen"),
        lwd = c(2, 2), lty = c(1, 1))