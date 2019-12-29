### graphing multiple securities
### 10/31 lecture


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

## find range/limit for y variables
range(all_adjusted$JNJ.Adjusted)
  # 43.194    95.254
range(all_adjusted$GSPC.Adjusted)
  # 1022.58   2130.82
range(all_adjusted$PFE.Adjusted)
  # 10.076    31.006

## plot data
plot( x = index(all_adjusted), xlab = "Date",
      y = all_adjusted$GSPC.Adjusted, ylab = "Adjusted Close Price ($)",
      ylim = c(1020, 2140), type = "l", lwd = 2, lty = 1, col = "blueviolet", 
      main = "Johnson and Johnson (JNJ), S&P 500 (GSPC), and Pfizer (PFE)
Daily Adjusted Close Price
January 1, 2010 - December 31, 2015")

## add margin space - continue plot
par(mar = c(5, 5, 5, 5))
par(new = TRUE)

## graph jnj and pfe
plot( x = index(all_adjusted), xlab = "", xaxt = "n",
      y = all_adjusted$JNJ.Adjusted, ylab = "", yaxt = "n", ylim = c(0,100),
      type = "l", lwd = 2, lty = 3, col = "red")
lines( x = index(all_adjusted), y = all_adjusted$PFE.Adjusted,
       lwd = 2, col = "forestgreen", lty = 2)

## label right y axis
axis(4)
mtext("Adjusted Close Price ($)", side = 4, line = 3)

## add legend
legend( "topleft", c("JNJ - right", "PFE - right", "GSPC - left"),
      col = c("blueviolet","forestgreen", "red"),
      lwd = c(2, 2, 2), lty = c(1, 2, 3))
