# Old Analysis using percentage foreign ownership and analysis only on 10yr and 1mo

# This file uses EFFR_w, Liq_w, Mex_w, EFFR_m, Liq_m and Mex_m to run following IV regressions
# 1. 2SLS regression of bid-ask spreads (in bps) on foreign ownership (in percent points)
# 2. Using 1st stage fitted values as X in ARIMAX model on yields
# It returns the results in a list whose components are 
# Stage1 (first stage OLS), IVReg_1mo (2nd stage on 1mo spread), 
# IVReg_10y(2nd stage on 10y spread), Model_10y (ARIMAX with IV on 10yr yield),
# Model_1mo (ARIMAX with IV on 10yr yield)


library(ivreg)
library(forecast) #for using auto.arima and Arima
library(urca) # for ur.df

# Checking Relevance ------------------------------------------------------

##Plotting EFFR and Capital inflows
# EFFR_long = melt(IVRegData_w, id.vars = "Date")
# ggplot( data = EFFR_long, aes(x = Date, y = value, color = variable)) +
#   geom_line(linewidth = 1.25)



# Monthly Analysis --------------------------------------------------------

## Creating monthly dataframe ------------------------------------------------
IVRegData_m = EFFR_m
IVRegData_m$F_Own_pcnt = Mex_m$F_Own_p *100                                            # Adding columns from other datatables
IVRegData_m$IIP = diff(log(IIP[IIP$DATE <= as.Date("2023-12-01") &
                          IIP$DATE >= as.Date("2005-01-01"), "INDPRO"]), 
                       lag = 12) * 100            #calculating monthly growth rates as difference in log of IIP from its 12th lag
IVRegData_m[c('MPTBA',"GMXN10Y","TIIE")] = Mex_m[ ,c('MPTBA',"GMXN10Y", "TIIE")] * 100
IVRegData_m[c('BA1mo', 'BA10Y')] = na.approx(Liq_m[c('BA_TBA', 'BA_10Y')]*100) 
IVRegData_m = IVRegData_m[IVRegData_m$Date <= as.Date("2022-12-31"),]

## Running ADF tests and creating first differenced data -----------------------
## on monthly data ---------------------------------------------

ADFresults =  apply(IVRegData_m[-1], MARGIN = 2, FUN = ur.df, type = "none")
ADFresults =  c(ADFresults, apply(apply(IVRegData_m[-1], 2, diff), 
                                  MARGIN = 2, FUN = ur.df, type = "none") )
for (i in 1:length(ADFresults)){
  if(i<= ncol(IVRegData_m[-1])){
    print(paste("p-value of ADF stat at levels for",colnames(IVRegData_m)[i+1],"is", 
                ADFresults[[i]]@testreg$coefficients[1,4]))
  }else{
    print(paste("p-value of ADF stat at 1st diff for",colnames(IVRegData_m)[i+2-ncol(IVRegData_m)],"is", 
                ADFresults[[i]]@testreg$coefficients[1,4]))
  }
}

# from the results, only 1 mo BA spread and IIP is stationary.

#taking first differences of EFFR and FO
IVData_m_stat = IVRegData_m[-1,] #to copy the dates and stationary columns, other columns get modified in next line
IVData_m_stat[-c(1,4,8)] = apply(IVRegData_m[-c(1,4,8)], 2, diff)


## First stage regressions -------------------------------------------------

### with 1st differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~ 0 + EFFR + IIP, data = IVData_m_stat )
summary(Stage1)
### without differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~  EFFR, data = IVRegData_m)
summary(Stage1)
### with lags -------------------------------------------
Stage1 = lm(F_Own_pcnt[-(1:4)]~  EFFR[-(1:4)] + EFFR[-c(1:3, 204)]
            + EFFR[-c(1:2, 203:204)]+ EFFR[-c(1, 202:204)]+ EFFR[-c(201:204)]
            , data = IVRegData_m )
summary(Stage1)
### with 1st differencing and lags -------------------------------------------
Stage1 = lm(F_Own_pcnt[-(1:4)]~ 0 + EFFR[-(1:4)] + EFFR[-c(1:3, 203)]
            + EFFR[-c(1:2, 202:203)]+ EFFR[-c(1, 201:203)]+ EFFR[-c(200:203)]
            , data = IVData_m_stat )
summary(Stage1)

## Second stage regressions -------------------------------------------------

## 2SLS ARIMAX on yields ------------------------------------------------------

#### with 1st differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~ 0 + EFFR +IIP , data = IVData_m_stat )
Model_10y = lm(IVData_m_stat$GMXN10Y ~ 0+ Stage1$fitted.values + IVData_m_stat$TIIE) #after differencing, this is a white noise. auto.arima suggests (0,1,0) on levels data
summary(Model_10y)

Model_1mo = Arima(IVData_m_stat$MPTBA, order = c(1,0,0),
                  xreg = cbind(Stage1$fitted.values,IVData_m_stat$TIIE), include.mean = F)
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs

#### without differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~ EFFR, data = IVRegData_m )
#auto.arima(IVRegData_m$GMXN10Y)
Model_10y = Arima(IVRegData_m$GMXN10Y, order = c(1,0,0),
                  xreg = Stage1$fitted.values)
Model_10y
(1-pnorm(abs(Model_10y$coef)/sqrt(diag(Model_10y$var.coef))))*2                 #calculating p-value


Model_1mo = Arima(IVRegData_m$MPTBA, order = c(1,0,0),
                  xreg = Stage1$fitted.values, include.mean = F)
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs

### 2SLS on spread ----------------------------------------------------------

#### On  Levels --------------------------------------------------------------
IVreg_1mo = ivreg(BA1mo ~ F_Own_pcnt | EFFR, data = IVRegData_m )
summary(IVreg_1mo)
IVreg_10y = ivreg(BA10Y ~ F_Own_pcnt | EFFR, data = IVRegData_m )
summary(IVreg_10y)

#### On  differences -----------------------------------------------------------
IVreg_1mo = ivreg(BA1mo ~  F_Own_pcnt | EFFR + IIP, data = IVData_m_stat )
summary(IVreg_1mo)
IVreg_10y = ivreg(BA10Y ~ 0 + F_Own_pcnt | EFFR + IIP, data = IVData_m_stat )
summary(IVreg_10y)




# Weekly Analysis ---------------------------------------------------------

## Creating weekly dataframe ------------------------------------------------
IVRegData_w = EFFR_w
IVRegData_w$F_Own_pcnt = Mex_w$F_Own_p *100                                            # Adding columns from other datatables
IVRegData_w[c('MPTBA',"GMXN10Y","TIIE")] = 
  na.approx(Mex_w[ ,c('MPTBA',"GMXN10Y","TIIE")]) * 100
#BA10y variable has many NAs between 2007 Oct and 2011 July and BA1y has few NA 
# values before 2023 Mar. So, analysis is done until 2022 Dec.
IVRegData_w[c('BA1mo', 'BA10Y')] = na.approx(Liq_w[c('BA_TBA', 'BA_10Y')]*100) 
IVRegData_w = IVRegData_w[IVRegData_w$Date <= as.Date("2022-12-31"),]

## Running ADF and creating first difference ----------------------------------

ADFresults =  apply(IVRegData_w[-1], MARGIN = 2, FUN = ur.df, type = "none")
ADFresults =  c(ADFresults, apply(apply(IVRegData_w[-1], 2, diff), 
                                  MARGIN = 2, FUN = ur.df, type = "none") )
for (i in 1:length(ADFresults)){
  if(i<7){
    print(paste("p-value of ADF stat at levels for",colnames(IVRegData_w)[i+1],"is", 
                ADFresults[[i]]@testreg$coefficients[1,4]))
  }else{
    print(paste("p-value of ADF stat at 1st diff for",colnames(IVRegData_w)[i+1],"is", 
                ADFresults[[i]]@testreg$coefficients[1,4]))
  }
}

# from the results, I can see that there is unit root in EFFR, F_Own_pcnt and yields. 
# the BA spreads are both stationary.

#taking first differences of EFFR and FO
IVData_w_stat = IVRegData_w[-1,] #to copy the dates and BA columns, other columns get modified in next line
IVData_w_stat[2:5] = apply(IVRegData_w[2:5], 2, diff)

# 2SLS on Weekly data  ------------------------------------------------------

## with 1st differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~ 0 + EFFR, data = IVData_w_stat )
summary(Stage1)

IVreg_1mo = ivreg(BA1mo ~ 0 + F_Own_pcnt | EFFR, data = IVData_w_stat )
summary(IVreg_1mo)
IVreg_10y = ivreg(BA10Y ~ 0 + F_Own_pcnt | EFFR, data = IVData_w_stat )
summary(IVreg_10y)

## without differencing ----------------------------------------------
Stage1 = lm(F_Own_pcnt~  EFFR, data = IVRegData_w)
summary(Stage1)

IVreg_1mo = ivreg(BA1mo ~  F_Own_pcnt | EFFR, data = IVRegData_w )
summary(IVreg_1mo)
IVreg_10y = ivreg(BA10Y ~  F_Own_pcnt | EFFR, data = IVRegData_w )
summary(IVreg_10y)

## with lags -------------------------------------------
Stage1 = lm(F_Own_pcnt[-(1:4)]~  EFFR[-(1:4)] + EFFR[-c(1:3, 887)]
            + EFFR[-c(1:2, 886:887)]+ EFFR[-c(1, 885:887)]+ EFFR[-c(884:887)]
            , data = IVRegData_w )
summary(Stage1)

## with 1st differencing and lags -------------------------------------------
Stage1 = lm(F_Own_pcnt[-(1:4)]~ 0 + EFFR[-(1:4)] + EFFR[-c(1:3, 886)]
            + EFFR[-c(1:2, 885:886)]+ EFFR[-c(1, 884:886)]+ EFFR[-c(883:886)]
            , data = IVData_w_stat )
summary(Stage1)


# 2SLS ARIMAX on yields ------------------------------------------------------

# #Checking ARIMA structure for 10yr and 1mo yield.
# lapply(Mex_w[,c('MPTBA',"GMXN10Y")], auto.arima, max.d = 1, xreg = Stage1$fitted.values) #suggest 1,1,1 for 1mo and 0,1,5 for 10yr
# 
# for(i in 1:3){
#   test_10y = arima(Mex_w$GMXN10Y, order = c(0,1,i))
#   print(test_10y)
#   print(BIC(test_10y))
# }                                                                                 #tested AR(i), MA(i) and conclude that AR(1,1,0) is best
# 
# #testing Unit root in fitted Foreign Ownership
# summary(ur.df(Stage1$fitted.values, type = "none"))
# summary(ur.df(diff(Stage1$fitted.values), type = "none"))                                #there is unit root

# Estimating ARIMAX model --------------------------------------------------

Model_10y = Arima(IVData_w_stat$GMXN10Y, order = c(1,0,0), 
                  xreg = diff(Stage1$fitted.values), include.mean = F )
Model_10y
(1-pnorm(abs(Model_10y$coef)/sqrt(diag(Model_10y$var.coef))))*2                 #calculating p-value
Model_10y$nobs

# Although AIC suggested ARIMA(1,1,1) for 1mo, I use ARIMA(1,1,0) because the 
# coefficients have similar magnitude and opposite signs indicating spurious regression
Model_1mo = Arima(IVData_w_stat$MPTBA, order = c(1,0,0),
                  xreg = diff(Stage1$fitted.values), include.mean = F)
Model_1mo
(1-pnorm(abs(Model_1mo$coef)/sqrt(diag(Model_1mo$var.coef))))*2                 #calculating p-value
Model_1mo$nobs

# Removing unnecessary variables ------------------------------------------
IVRegResults = list(Stg1 = Stage1, Stg2_1mo = IVreg_1mo, Stg2_10y = IVreg_10y,
                    ARIMAX1mo = Model_1mo, ARIMAX10y = Model_10y)

rm(IVRegData_w,IVRegData_m, IVData_w_stat , IVData_m_stat, ADFresults,Stage1, 
   IVreg_1mo,IVreg_10y, Model_1mo, Model_10y)
