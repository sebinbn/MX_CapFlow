# This code estimates ARIMAX model where F_Own_p is the X variable.It uses Mex_w
# for weekly data.


library(forecast) #for using auto.arima

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

Model_10y = Arima(Mex_w$GMXN10Y, order = c(1,1,0), xreg = Mex_w$F_Own_p )
Model_10y
(1-pnorm(abs(Model_10y$coef)/sqrt(diag(Model_10y$var.coef))))*2                 #calculating p-value
Model_10y$nobs

Model_1mo = Arima(Mex_w$MPTBA, order = c(1,1,1), xreg = Mex_w$F_Own_p )
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs
