# This file uses EFFR_w, Liq_w and Mex_w to run following IV regressions
# 1. 2SLS regression of bid-ask spreads (in bps) on foreign ownership (in percent points)
# 2. Using 1st stage fitted values as X in ARIMAX model on yields


library(ivreg)
library(forecast) #for using auto.arima and Arima
library(urca) # for ur.df

# Checking Relevance ------------------------------------------------------

##Plotting EFFR and Capital inflows
# EFFR_long = melt(IVRegData, id.vars = "Date")
# ggplot( data = EFFR_long, aes(x = Date, y = value, color = variable)) +
#   geom_line(linewidth = 1.25)


# 2SLS - IV ------------------------------------------------------------

## Creating dataframe ------------------------------------------------

IVRegData$F_Own_pcnt = Mex_w$F_Own_p *100                                            # Adding columns from other datatables
#BA10y variable has many NAs between 2007 Oct and 2011 July and BA1y has few NA 
# values before 2023 Mar. So, analysis is done until 2022 Dec.
IVRegData[c('BA1mo', 'BA10Y')] = na.approx(Liq_w[c('BA_TBA', 'BA_10Y')]*100) 
IVRegData = IVRegData[IVRegData$Date < as.Date("2022-12-31"),]


## Running ADF test --------------------------------------------------------

ADFresults =  apply(IVRegData[-1], MARGIN = 2, FUN = ur.df, type = "none")
for (i in 1:4){
  print(paste("p-value of ADF stat at levels for",colnames(IVRegData)[i+1],"is", 
              ADFresults[[i]]@testreg$coefficients[1,4]))
}

# from the results, I can see that there is unit root in EFFR and F_Own_pcnt. 
# the BA spreads are both stationary.

# 2SLS on BA Spread  ------------------------------------------------------

Stage1 = lm(F_Own_pcnt~ EFFR, data = IVRegData)
Stage2 = lm(IVRegData$BA1mo ~ Stage1$fitted.values)
summary(Stage1)

IVreg_1mo = ivreg(BA1mo ~ F_Own_pcnt | EFFR, data = IVRegData)
summary(IVreg_1mo)
IVreg_10y = ivreg(BA10Y ~ F_Own_pcnt | EFFR, data = IVRegData)
summary(IVreg_10y)

# 2SLS ARIMAX on yields ------------------------------------------------------

#Checking ARIMA structure for 10yr and 1mo yield.
lapply(Mex_w[,c('MPTBA',"GMXN10Y")], auto.arima, max.d = 1, xreg = Stage1$fitted.values) #suggest 1,1,1 for 1mo and 0,1,5 for 10yr

for(i in 1:3){
  test_10y = arima(Mex_w$GMXN10Y, order = c(0,1,i))
  print(test_10y)
  print(BIC(test_10y))
}                                                                                 #tested AR(i), MA(i) and conclude that AR(1,1,0) is best

#testing Unit root in fitted Foreign Ownership
summary(ur.df(Stage1$fitted.values, type = "none"))
summary(ur.df(diff(Stage1$fitted.values), type = "none"))                                #there is unit root

# Estimating ARIMAX model --------------------------------------------------

Model_10y = Arima(Mex_w$GMXN10Y, order = c(1,1,0), xreg = Stage1$fitted.values )
Model_10y
(1-pnorm(abs(Model_10y$coef)/sqrt(diag(Model_10y$var.coef))))*2                 #calculating p-value
Model_10y$nobs

# Although AIC suggested ARIMA(1,1,1) for 1mo, I use ARIMA(1,1,0) because the 
# coefficients have similar magnitude and opposite signs indicating spurious regression
Model_1mo = Arima(Mex_w$MPTBA, order = c(1,1,0), xreg = Stage1$fitted.values )
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs

