# This code estimates ARIMAX model where F_Own_p is the X variable.It uses Mex_w
# for weekly data on yields and Liq_w for weekly data on bid-ask spread.


library(forecast) #for using auto.arima and Arima

# Model Selection ---------------------------------------------------------

#Checking ARIMA structure for 10yr and 1mo yield.
lapply(Mex_w[,c('MPTBA',"GMXN10Y")], auto.arima, max.d = 1, xreg = Mex_w$F_Own_p) #suggest 1,1,1 for 1mo and 0,1,5 for 10yr

for(i in 1:3){
  test_10y = arima(Mex_w$GMXN10Y, order = c(0,1,i))
print(test_10y)
print(BIC(test_10y))
}                                                                                 #tested AR(i), MA(i) and conclude that AR(1,1,0) is best

#testing Unit root for F_Own_p
summary(ur.df(Mex_w$F_Own_p, type = "none"))
summary(ur.df(diff(Mex_w$F_Own_p), type = "none"))                                #there is unit root

# Estimating ARIMAX model --------------------------------------------------

Model_10y = Arima(Mex_w$GMXN10Y, order = c(1,1,0), xreg = Mex_w$F_Own_p*100 )
Model_10y
(1-pnorm(abs(Model_10y$coef)/sqrt(diag(Model_10y$var.coef))))*2                 #calculating p-value
Model_10y$nobs

# Although AIC suggested ARIMA(1,1,1), I use ARIMA(1,1,0) because the coefficients
# have similar magnitude and opposite signs indicating spurious regression
Model_1mo = Arima(Mex_w$MPTBA, order = c(1,1,0), xreg = Mex_w$F_Own_p*100 )
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs


# Regressing liquidity impact-----------------------------------

Liq_w$F_Own_p = Mex_w$F_Own_p

Liq_w[is.na(Liq_w$BA_TBA),c('Date', 'BA_TBA')] #most missing values are 2023 March onwards
Liq_w[is.na(Liq_w$BA_10Y),c('Date', 'BA_10Y')] # all missing values between 2007 Oct - 2011 July

lapply(Liq_w[,c('BA_TBA',"BA_10Y")], auto.arima, max.d = 0)

summary(ur.df(na.omit(Liq_w$BA_TBA), type = "none",selectlags = "AIC")) #conclude stationarity
summary(ur.df(na.omit(Liq_w$BA_10Y), type = "none",selectlags = "AIC")) #conclude stationarity

Liq_reg = lapply(Liq_w[,c('BA_TBA',"BA_TBF","BA_01Y","BA_02Y","BA_05Y","BA_10Y", "BA_30Y")], 
       function(x){summary(lm(x~F_Own_p, data = Liq_w))} )

