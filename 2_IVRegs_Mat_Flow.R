
# This file uses EFFR_m, Liq_m and Mex_m to run following IV regressions across 
# various maturities to find the impact of changes in total value of foreign 
# ownership unlike earlier analysis which used share of foreign ownership.

# 1. 2SLS regression of bid-ask spreads (in bps) on value foreign owned bonds (in millions of Pesos)
# 2. Using 1st stage fitted values as X in ARIMAX model on yields


library(ivreg)
library(forecast) #for using auto.arima and Arima
library(urca) # for ur.df


# Monthly Analysis --------------------------------------------------------

## Creating monthly dataframe ------------------------------------------------
IVRegData_m = EFFR_m
# Adding columns from other datatables
IVRegData_m["TIIE"] = Mex_m["TIIE"]*100
IVRegData_m["F_Own"] = Mex_m["F_Own"]/1000 #converting to billions of Pesos 
IVRegData_m$IIP = diff(log(IIP[IIP$DATE <= as.Date("2023-12-01") &
                                 IIP$DATE >= as.Date("2005-01-01"), "INDPRO"]), 
                       lag = 12) * 100            #calculating monthly growth rates as difference in log of IIP from its 12th lag
IVRegData_m[colnames(Mex_m)[2:19]] = na.approx(Mex_m[colnames(Mex_m)[2:19]]) * 100
IVRegData_m[colnames(Liq_m)[38:55]] = na.approx(Liq_m[colnames(Liq_m)[38:55]])*100
IVRegData_m["F_Own"]
IVRegData_m = IVRegData_m[IVRegData_m$Date <= as.Date("2022-12-31"),]
colMeans(is.na(IVRegData_m)) #values which are missing at the beginning or end stay missing.

## Running ADF tests and creating first differenced data -----------------------
## on monthly data ---------------------------------------------

ADFresults_IV = data.frame(Var = rep("", ncol(IVRegData_m)-1, 1), 
                           ADF = matrix(NaN, ncol(IVRegData_m)-1, 4))
colnames(ADFresults_IV)[-1] = c("stat_lev","stat_diff", "pval_lev","pval_diff")
for (i in 1:ncol(IVRegData_m)-1){
  ADF_level = ur.df(na.omit(IVRegData_m[,i+1]), type = "none")
  ADF_diff = ur.df(diff(na.omit(IVRegData_m[,i+1])), type = "none")
  
  ADFresults_IV[i,1] = colnames(IVRegData_m)[i+1]
  ADFresults_IV[i,2] = ADF_level@teststat
  ADFresults_IV[i,3] = ADF_diff@teststat
  ADFresults_IV[i,4] = ADF_level@testreg$coefficients[1,4]
  ADFresults_IV[i,5] = ADF_diff@testreg$coefficients[1,4]
}

# from the results, only 1 week, 1,3,6,9 mo,1Y BA spread and IIP is stationary.

#taking first differences
IVData_m_stat = IVRegData_m[-1,] #to copy the dates and stationary columns, other columns get modified in next line
ColstoDiff = !colnames(IVRegData_m) %in% c("Date","IIP", "BA_TBA", "BA_TBC","BA_TBF",
                                           "BA_TBI","BA_01Y","BA_TB1")
IVData_m_stat[ColstoDiff] = apply(IVRegData_m[ColstoDiff], 2, diff)


## First stage regressions -------------------------------------------------

### with 1st differencing ----------------------------------------------
Stage1 = lm(F_Own ~ 0 + EFFR + IIP, data = IVData_m_stat )
summary(Stage1)

Stg1_F = summary(Stage1)$fstatistic["value"] 
tFCorr = 1.935 + (8.473 - Stg1_F)/(8.473 - 8.196)* (1.98-1.935)
# Calculation of tF se based on Stage 1 F-stat has to be manually done. Current 
# calculation is based on F-value of 8.39

## Second stage regressions -------------------------------------------------

### 2SLS ARIMAX on yields ------------------------------------------------------

# creating table to store p,d,q of ARIMA(p,d,q) fit for each yield by auto.arima
AutoAR_results =data.frame(Var = colnames(IVRegData_m)[6:23], PDQ= matrix(NaN,18,3))
Y_results = list() #initiating list to store yield analysis results
Y_FO_results = data.frame(Var = colnames(IVRegData_m)[6:23], coef = matrix(NaN,18,1),
                          se = matrix(NaN,18,1), tFse = matrix(NaN,18,1), pValue = matrix(NaN,18,1) )
#### with 1st differencing ----------------------------------------------
for (i in 6:23){
  AutoAR = auto.arima(IVRegData_m[,i])
  AutoAR_results[i-5,2:4] = arimaorder(AutoAR)
  if( all(AutoAR_results[i-5,2:4] == c(0,1,0)) ){
    Y_result = lm(IVData_m_stat[,i] ~ 0+ Stage1$fitted.values + IVData_m_stat$TIIE)
    Y_FO_results$coef[i-5] = Y_result$coefficients[1]
    Y_FO_results$se[i-5] = sqrt(diag(vcov(Y_result)))[1]
  }else{
    Y_result = Arima(IVData_m_stat[,i], order = c(1,0,0),
                     xreg = cbind(Stage1$fitted.values,IVData_m_stat$TIIE), include.mean = F)
    Y_FO_results$coef[i-5] = Y_result$coef[2]
    Y_FO_results$se[i-5] = sqrt(diag(Y_result$var.coef))[2]
  }
  res_name = colnames(IVRegData_m)[i]
  Y_results[[res_name]] = Y_result
  
}

Y_FO_results$tFse = Y_FO_results$se*tFCorr
Y_FO_results$pValue = (1-pnorm(abs(Y_FO_results$coef)/Y_FO_results$tFse))*2

#displaying results of 30Y and 1mo for filling up table
summary(Y_results$GMXN30Y)
Y_results$MPTBA
(1-pnorm(abs(Y_results$MPTBA$coef)/sqrt(diag(Y_results$MPTBA$var.coef))))*2

## Reduced form regressions on yields ------------------------------------------

#Running reduced form OLS on 30yr and 1mo yields
OLS30yr = lm(GMXN30Y ~ 0 + EFFR + TIIE + IIP, data = IVData_m_stat)
summary(OLS30yr)

OLS1mo = Arima(IVData_m_stat$MPTBA, order = c(1,0,0), 
               xreg = data.matrix(IVData_m_stat[,c("EFFR","IIP","TIIE")]),
               include.mean = F)
OLS1mo
(1-pnorm(abs(OLS1mo$coef)/sqrt(diag(OLS1mo$var.coef))))*2                 #calculating p-value



### 2SLS on spread ----------------------------------------------------------

S_results = list() #initiating list to store spread analysis results
S_FO_results = data.frame(Var = colnames(IVRegData_m)[24:41], coef = matrix(NaN,18,1),
                          se = matrix(NaN,18,1) )
#### with 1st differencing ----------------------------------------------
for (i in 24:41){
  if( colnames(IVData_m_stat)[i] %in% c("BA_TBA","BA_TBF","BA_TBC","BA_TBI","BA_TB1","BA_01Y") ){
    regformula = paste(colnames(IVData_m_stat)[i], "~ F_Own | EFFR + IIP" )
    pick = 2
  }else{
    regformula = paste(colnames(IVData_m_stat)[i], "~ 0 + F_Own | EFFR + IIP" )
    pick = 1
  }
  S_result = ivreg(regformula, data = IVData_m_stat )
  S_FO_results[i -23,"coef"] = S_result$coefficients[pick]
  S_FO_results[i -23,"se"] = sqrt(diag(vcov(S_result)))[pick]
  res_name = colnames(IVRegData_m)[i]
  S_results[[res_name]] = S_result
  
}
S_FO_results$tFse = S_FO_results$se*tFCorr

#displaying results of 30Yr and 1mo for filling in table
summary(S_results[["BA_TBA"]])
summary(S_results[["BA_30Y"]])

## Reduced form regressions on spreads ------------------------------------------
OLS30yr_S = lm(BA_30Y ~  0 + EFFR + TIIE + IIP , data = IVData_m_stat)
summary(OLS30yr_S)

OLS1mo_S = lm(BA_TBA ~  EFFR + TIIE + IIP , data = IVData_m_stat)
summary(OLS1mo_S)


# Removing Unnecessary variables ------------------------------------------

rm(ADF_level, ADF_diff, ColstoDiff, Y_result, S_result, res_name, regformula,
   pick)
