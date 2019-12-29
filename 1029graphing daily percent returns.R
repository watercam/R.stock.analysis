### graping daily percent returns


install.packages("quantmod")
install.packages("xts")
library(quantmod)
library(xts)

## get data from yahoo
jnj <- getSymbols("JNJ", src = "yahoo", auto.assign = FALSE)
gspc <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
pfe <- getSymbols("PFE", src = "yahoo", auto.assign = FALSE)

## subset data
jnj_subset <- subset(jnj[ , 6], index(jnj) >= "2010-01-01" & 
                       index(jnj) <= "2015-12-31")
gspc_subset <- subset(gspc[ , 6], index(gspc) >= "2010-01-01" & 
                       index(gspc) <= "2015-12-31")
pfe_subset <- subset(pfe[ , 6], index(pfe) >= "2010-01-01" & 
                       index(pfe) <= "2015-12-31")

## merge data
all_adjusted <- na.locf(merge(pfe_subset, jnj_subset))
all_adjusted <- na.locf(merge(all_adjusted, gspc_subset))

## calculate % return for each security
all_adjusted$JNJ.Ret <- Delt(all_adjusted$JNJ.Adjusted) * 100
all_adjusted$PFE.Ret <- Delt(all_adjusted$PFE.Adjusted) * 100
all_adjusted$GSPC.Ret <- Delt(all_adjusted$GSPC.Adjusted) * 100

## find min and max data values
summary(all_adjusted$JNJ.Ret)
# -3.21    +5.38
summary(all_adjusted$PFE.Ret)
# -4.75    +5.64
summary(all_adjusted$GSPC.Ret)
# -6.66    +4.74
## use range of -8 to +7

## plot data
plot( x = index(all_adjusted), xlab = "Date",
      y = all_adjusted$JNJ.Ret, ylab = "Daily Return (%)", ylim = c(-8,7),
      type = "l", col = "blueviolet", lwd = 2, lty = 1,
      main = "Johnson and Johnson (JNJ), SP 500 (GPSC), Pfizer (PFE)
Daily Return from Adjusted Close
January 1, 2010 - December 31, 2015")

## add gspc and pfe
lines( x = index(all_adjusted), y = all_adjusted$GSPC.Ret, lwd = 2, lty = 2,
       col = "forestgreen")
lines( x = index(all_adjusted), y = all_adjusted$PFE.Ret, lwd = 2, lty = 3,
       col = "red")

## add legend
legend( "bottomright", c("JNJ", "GSPC", "PFE"),
        col = c("blueviolet","forestgreen", "red"),
        lwd = c(2, 2, 2), lty = c(1, 2, 3))
