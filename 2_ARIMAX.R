# CCFs -------------------------------------------------------------------------

#Checking ARIMA structure for 10yr and 1mo yield.
lapply(Mex2[,c(8,9)], auto.arima) #suggest 0,1,0 for both
lapply(Mex1[,c(8,9)], auto.arima) #suggest 0,1,2 for 1mo and 0,1,0 for 10y

#generating CCFs on first difference for weekly data
ccf(diff(Mex2[,"r10y_i"]), diff(Mex2[,"r1mo_i"]),lag.max = 5,ylab = "CCF", xlab = "")
ccf(diff(Mex1[,"r10y_i"]), diff(Mex1[,"r1mo_i"]),lag.max = 5,ylab = "CCF", xlab = "")

#generating CCFs on first difference for daily data
ccf_d_h = ccf(diff(Mex2[,"r10y"]), diff(Mex2[,"r1mo"]),lag.max = 5,ylab = "CCF", xlab = "")
ccf_d_l = ccf(diff(Mex1[,"r10y"]), diff(Mex1[,"r1mo"]),lag.max = 5,ylab = "CCF", xlab = "")


acf(na.omit(diff(slope101[date_pre]))) #ACF and PACF at 1st lag significant
ar101 = arima(slope101[date_pre], order = c(0,1,1)) #This is better than AR1. ARMA11 makes both insignificant.
ar101
acf2(ar101$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(slope101[date_pre], model = ar101))
fitwhite1 = residuals(Arima(all_ts$WACR[date_pre], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[date_pre], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[date_pre], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[date_pre], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "", main = "10yr-1yr ~ US10yr", lag.max = 15)

# ARIMAX model for 10yr yield --------------------------------------------------
Mex_w_ar = Mex_w[Mex_w$Date <= as.Date("2019-07-01"),]
auto.arima(Mex_w_ar$Prop_FO)
auto.arima(Mex_w$r10y)
auto.arima(Mex_w[Mex_w$Date < as.Date("2023-08-01"),"r1mo"])
auto.arima(Mex_w$TIIE)
soln = arima(Mex_w$r10y, order = c(3,1,0), xreg = Mex_w$Prop_FO )
(1-pnorm(abs(soln$coef)/sqrt(diag(soln$var.coef))))*2

soln = arima(Mex_w[Mex_w$Date < as.Date("2023-08-01"),"r1mo"],order = c(1,1,0),
             xreg = Mex_w[Mex_w$Date < as.Date("2023-08-01"),"Prop_FO"] )         #subsetting as values are missing Aug 2023 onwards
soln$nobs

soln = arima(Mex_w$TIIE, order = c(0,1,3), xreg = Mex_w$Prop_FO )
soln
summary(ur.df(diff(Mex_w$TIIE), type = "none"))