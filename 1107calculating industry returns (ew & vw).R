####################################
### calculating industry returns ###
### 11/07 lecture ##################
####################################

install.packages("quantmod")
install.packages("xts")
library(quantmod)
library(xts)

## get data from yahoo
jnj <- getSymbols("JNJ", src = "yahoo", auto.assign = FALSE)
gspc <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
pfe <- getSymbols("PFE", src = "yahoo", auto.assign = FALSE)

## create subsets of data
jnj_subset <- subset(jnj[ , 6], index(jnj) >= "2010-01-01" & 
                       index(jnj) <= "2015-12-31")
gspc_subset <- subset(gspc[ , 6], index(gspc) >= "2010-01-01" & 
                        index(gspc) <= "2015-12-31")
pfe_subset <- subset(pfe[ , 6], index(pfe) >= "2010-01-01" & 
                       index(pfe) <= "2015-12-31")

## merge data into one database
all_adjusted <- na.locf(merge(pfe_subset, jnj_subset))
all_adjusted <- na.locf(merge(all_adjusted, gspc_subset))

## normalize data, convert from xts to dataframe
#remove na.omit values from dataset
all_adjusted_nona <- na.omit(all_adjusted)
#convert dates into vector
date <- index(all_adjusted_nona)
#combine dates vector and dataset
all_adjusted_df <- cbind(data.frame(date), data.frame(all_adjusted_nona))

## calculate normalized values by referencing row 1 for each asset
all_adjusted_df$JNJ.Norm <- all_adjusted_df$JNJ.Adjusted / 
  all_adjusted_df$JNJ.Adjusted[1]
all_adjusted_df$GSPC.Norm <- all_adjusted_df$GSPC.Adjusted / 
  all_adjusted_df$GSPC.Adjusted[1]
all_adjusted_df$PFE.Norm <- all_adjusted_df$PFE.Adjusted / 
  all_adjusted_df$PFE.Adjusted[1]

## replace all above code with load(all_adjusted_df)

## acquire peer group data; subset to period of interest; merge
pfe <- getSymbols( "PFE", src = "yahoo", auto.assign = FALSE)
nvs <- getSymbols( "NVS", src = "yahoo", auto.assign = FALSE)
mrk <- getSymbols( "MRK", src = "yahoo", auto.assign = FALSE)
pfe_subset <- subset(pfe[ , 6], index(pfe) >= "2010-01-01" &
                       index(pfe) <= "2015-12-31")
nvs_subset <- subset(nvs[ , 6], index(nvs) >= "2010-01-01" &
                       index(nvs) <= "2015-12-31")
mrk_subset <- subset(mrk[ , 6], index(mrk) >= "2010-01-01" &
                       index(mrk) <= "2015-12-31")
peer <- merge(pfe_subset, nvs_subset)
peer <- merge(peer, mrk_subset)

## calculate returns
peer$pfe.ret <- Delt(peer$PFE.Adjusted) * 100
peer$nvs.ret <- Delt(peer$NVS.Adjusted) * 100
peer$mrk.ret <- Delt(peer$MRK.Adjusted) * 100

## EQUAL WEIGHTED: weight = 1/number of companies in index
peer$w_pfe <- 1/3
peer$w_nvs <- 1/3
peer$w_mrk <- 1/3

## calculate index return with peer returns and weights
peer$ew_ret <- (peer$w_pfe * peer$pfe.ret) + (peer$w_nvs * peer$nvs.ret) + (peer$w_mrk * peer$mrk.ret)

## remove NA values; name variable; bind ew returns to all df
peer_na <- na.omit(peer)
peer_df <- data.frame(peer_na)
peer_ret_df <- cbind(all_adjusted_df, peer_df$ew_ret)

## VALUE WEIGHTED: weight = individual mktcap / index mktcap

pfe <- getSymbols( "PFE", src = "yahoo", auto.assign = FALSE)
nvs <- getSymbols( "NVS", src = "yahoo", auto.assign = FALSE)
mrk <- getSymbols( "MRK", src = "yahoo", auto.assign = FALSE)

pfe_subset <- subset(pfe[ , c(4,6)], index(pfe) >= "2010-01-01" &
                       index(pfe) <= "2015-12-31")
nvs_subset <- subset(nvs[ , c(4,6)], index(nvs) >= "2010-01-01" &
                       index(nvs) <= "2015-12-31")
mrk_subset <- subset(mrk[ , c(4,6)], index(mrk) >= "2010-01-01" &
                       index(mrk) <= "2015-12-31")

peer_vw <- merge(pfe_subset, nvs_subset)
peer_vw <- merge(peer_vw, mrk_subset)

## convert from xts to dataframe; combine date and data; number each row
date <- index(peer_vw)
peer_vw_df <- cbind(data.frame(date), data.frame(peer_vw))
rownames(peer_vw_df) <- seq(1:nrow(peer_vw_df))

## write a for() loop with nested if(), else if(), else() statements
for(i in 1:nrow(peer_vw_df)) {
  if(peer_vw_df$date[i] <= as.Date("2010-02-18")) {
    peer_vw_df$pfe.shrout[i] <- 8070372772
  } else if(peer_vw_df$date[i] <= as.Date("2011-02-22")) {
    peer_vw_df$pfe.shrout[i] <- 7995220402
  } else if(peer_vw_df$date[i] <= as.Date("2012-02-21")) {
    peer_vw_df$pfe.shrout[i] <- 7538520276
  } else if(peer_vw_df$date[i] <= as.Date("2013-02-21")) {
    peer_vw_df$pfe.shrout[i] <- 7189061853  
  } else if(peer_vw_df$date[i] <= as.Date("2014-02-21")) {
    peer_vw_df$pfe.shrout[i] <- 6382925343  
  } else if(peer_vw_df$date[i] <= as.Date("2015-02-20")) {
    peer_vw_df$pfe.shrout[i] <- 6128855392 
  } else {
    peer_vw_df$pfe.shrout[i] <- 6184139991
  }}

for(i in 1:nrow(peer_vw_df)) {
  if(peer_vw_df$date[i] <= as.Date("2010-01-26")) {
    peer_vw_df$nvs.shrout[i] <- 2274353351
  } else if(peer_vw_df$date[i] <= as.Date("2011-01-27")) {
    peer_vw_df$nvs.shrout[i] <- 2289445178
  } else if(peer_vw_df$date[i] <= as.Date("2012-01-25")) {
    peer_vw_df$nvs.shrout[i] <- 2406693857
  } else if(peer_vw_df$date[i] <= as.Date("2013-01-23")) {
    peer_vw_df$nvs.shrout[i] <- 2420620174
  } else if(peer_vw_df$date[i] <= as.Date("2014-01-29")) {
    peer_vw_df$nvs.shrout[i] <- 2426084308
  } else if(peer_vw_df$date[i] <= as.Date("2015-01-27")) {
    peer_vw_df$nvs.shrout[i] <- 2398626257
  } else {
    peer_vw_df$nvs.shrout[i] <- 2373894817
  }}

for(i in 1:nrow(peer_vw_df)) {
  if(peer_vw_df$date[i] <= as.Date("2010-02-18")) {
    peer_vw_df$mrk.shrout[i] <- 3115317260
  } else if(peer_vw_df$date[i] <= as.Date("2011-02-22")) {
    peer_vw_df$mrk.shrout[i] <- 3083080697
  } else if(peer_vw_df$date[i] <= as.Date("2012-02-21")) {
    peer_vw_df$mrk.shrout[i] <- 3044008396
  } else if(peer_vw_df$date[i] <= as.Date("2013-02-21")) {
    peer_vw_df$mrk.shrout[i] <- 3022367538
  } else if(peer_vw_df$date[i] <= as.Date("2014-02-21")) {
    peer_vw_df$mrk.shrout[i] <- 2940622461
  } else if(peer_vw_df$date[i] <= as.Date("2015-02-20")) {
    peer_vw_df$mrk.shrout[i] <- 2838192933
  }  else {
    peer_vw_df$mrk.shrout[i] <- 2775258591
  }}    

## create market cap variables for each firm (shrout * close)
peer_vw_df$pfe.mktcap <- peer_vw_df$PFE.Close * peer_vw_df$pfe.shrout
peer_vw_df$nvs.mktcap <- peer_vw_df$NVS.Close * peer_vw_df$nvs.shrout
peer_vw_df$mrk.mktcap <- peer_vw_df$MRK.Close * peer_vw_df$mrk.shrout

## sum of market cap
peer_vw_df$tot.mktcap <- peer_vw_df$pfe.mktcap + peer_vw_df$nvs.mktcap + peer_vw_df$mrk.mktcap

## calculate weights
peer_vw_df$w.pfe <- peer_vw_df$pfe.mktcap / peer_vw_df$tot.mktcap
peer_vw_df$w.nvs <- peer_vw_df$nvs.mktcap / peer_vw_df$tot.mktcap
peer_vw_df$w.mrk <- peer_vw_df$mrk.mktcap / peer_vw_df$tot.mktcap

## calculate return with Delt() * 100 to convert to percent
peer_vw_df$pfe.ret <- Delt(peer_vw_df$PFE.Adjusted) * 100
peer_vw_df$nvs.ret <- Delt(peer_vw_df$NVS.Adjusted) * 100
peer_vw_df$mrk.ret <- Delt(peer_vw_df$MRK.Adjusted) * 100

## calculate total index return
peer_vw_df$vw_ret <- (peer_vw_df$pfe.ret * peer_vw_df$w.pfe) + (peer_vw_df$nvs.ret * peer_vw_df$w.nvs) + (peer_vw_df$mrk.ret * peer_vw_df$w.mrk)

## omit na values; bind vw to ew dataframe
peer_vw_df <- na.omit(peer_vw_df)
peer_all_df <- cbind(data.frame(peer_ret_df), data.frame(peer_vw_df$vw_ret))

## create pegged chart with all variables pegged to JNJ starting price; subset relavent data
peg_chart <- peer_all_df[ , c(1, 2, 5, 11, 12)]
names(peg_chart) <- c("date", "JNJ.Price", "JNJ.Ret", "EW.Ret", "VW.Ret")

##
for(i in 1:nrow(peg_chart)) {
  if(peg_chart$date[i] == peg_chart$date[1]) {
    peg_chart$JNJ.peg[i] <- peg_chart$JNJ.Price[1]
    peg_chart$EW.peg[i] <- peg_chart$JNJ.Price[1]
    peg_chart$VW.peg[i] <- peg_chart$JNJ.Price[1]
  } else {
    peg_chart$JNJ.peg[i] <- peg_chart$JNJ.peg[i-1] * (1 + (peg_chart$JNJ.Ret/100))
    peg_chart$EW.peg[i] <- peg_chart$EW.peg[i-1] * (1 + (peg_chart$EW.Ret/100))
    peg_chart$VW.peg[i] <- peg_chart$VW.peg[i-1] * (1 + (peg_chart$VW.Ret/100))
  }}

## plot JNJ chart with pegged industry returns
plot ( x= peg_chart$date, xab = "Date",
       y = peg_chart$JNJ.peg, ylab = "Value of Investment ($)", type = "l",
       ylim = c(35,130), lwd = 2, lty = 2, col = "blueviolet",
       main = "Johnson and Johnson (JNJ) and an Equal-Weighted (EW) and Value-Weighted (VW)
       Portfolio of Peers Pegged to JNJ's Starting Adjusted CLose
       January 1, 2010 - December 31, 2015")
lines( x= peg_chart$date, y = peg_chart$EW.peg, lwd = 2, lty = 3, col = "palegreen3")
lines( x= peg_chart$date, y = peg_chart$VW.peg, lwd = 2, lty = 4, col = "orange")
abline ( h = 47.62, lty = 1, col = "black")
legend("bottomright", c("JNJ", "EW", "VW"), lwd = c(2,2,2), lty = c(2,3,4),
       col = c("blueviolet", "palegreen3", "orange"))