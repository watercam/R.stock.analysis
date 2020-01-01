#####################################
###### graphing a pegged chart ######
###### 11/05 lecture ################
#####################################

install.packages("quantmod")
install.packages("xts")
library(quantmod)
library(xts)

## data from yahoo
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

## plot values
plot( x = date, xlab = "Date",
      y = all_adjusted_df$JNJ.Norm, ylab = "Normalized Return (%)",
      ylim = c(.5,2.5), type = "l",
      col = "blueviolet", lwd = 2, lty = 1,
      main = "Johnson and Johnson (JNJ), SP 500 (GPSC), Pfizer (PFE)
Normalized Return from Adjusted Close
January 1, 2010 - December 31, 2015")

## add gspc and pfe
lines( x = date, y = all_adjusted_df$GSPC.Norm, lwd = 2, lty = 2,
       col = "forestgreen")
lines( x = date, y = all_adjusted_df$PFE.Norm, lwd = 2, lty = 3,
       col = "red")

## add legend
legend( "bottomright", c("JNJ", "GSPC", "PFE"),
        col = c("blueviolet","forestgreen", "red"),
        lwd = c(2, 2, 2), lty = c(1, 2, 3))

## alternate graph with horizontal line
plot ( x = all_adjusted_df$date, xlab = "Date",
       y = all_adjusted_df$JNJ.Norm, ylab = "Value of $1 investment ($)",
       type = "l", ylim = c(0, 2.5), lwd = 2, lty = 2, col = "blueviolet",
       main = "Johnson and Johnson (JNJ), Pfizer (PFE), SP 500 (GSPC)
       Value of $1 Investment Based on Daily Adjusted CLsoe Prices
       January 1, 2010 - December 31, 2015")
lines( x = all_adjusted_df$date, y = all_adjusted_df$GSPC.Norm,
       lwd = 2, lty = 3, col = "red")
lines( x = all_adjusted_df$date, y = all_adjusted_df$PFE.Norm,
       lwd = 2, lty = 4, col = "forestgreen")
legend("topleft", c("JNJ", "GSPC", "PFE"), lwd = c(2, 2, 2),
       lty = c(2, 3, 4), col = c("blueviolet", "red", "forestgreen"))
abline( h = 1, lty = 1, col = "black")

