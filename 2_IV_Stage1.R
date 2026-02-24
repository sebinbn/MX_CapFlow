# This file uses EFFR_m, Liq_m and Mex_m to run First stage regression. Results
# will be used in 2_Stage2_SURE.R



# Monthly Analysis --------------------------------------------------------

## Creating monthly dataframe ------------------------------------------------
IVRegData_m = EFFR_m
# Adding columns from other datatables
IVRegData_m["TIIE"] = Mex_m["TIIE"]*100
IVRegData_m["F_Own"] = Mex_m["F_Own"]/1000 #converting to billions of Pesos 
IVRegData_m$d_ln_IIP = diff(log(IIP[IIP$DATE <= as.Date("2023-12-01") &
                                      IIP$DATE >= as.Date("2005-01-01"), "INDPRO"]), 
                            lag = 12) * 100            #calculating monthly growth rates as difference in log of IIP from its 12th lag
IVRegData_m[colnames(Mex_m)[2:19]] = na.approx(Mex_m[colnames(Mex_m)[2:19]]) * 100

### Handling missing values ------------------------------------------------
#many short term yields missing in 2023. remove 2023 to avoid this.
IVRegData_m = IVRegData_m[IVRegData_m$Date <= as.Date("2022-12-31"),] 
# GMXN01YR available only from 2008-04-30. So data subsetted to start from this point.
# GMXN09YR is missing after 2018-05-31. these are filled in as the average of the 
# 8 and 10 year yields.
IVRegData_m = IVRegData_m[IVRegData_m$Date > as.Date("2008-04-15"),]
IVRegData_m$GMXN09Y[is.na(IVRegData_m$GMXN09Y)] = rowMeans(
  IVRegData_m[is.na(IVRegData_m$GMXN09Y), c("GMXN08Y", "GMXN10Y")])
colMeans(is.na(IVRegData_m))

## Running ADF tests and creating first differenced data -----------------------

ADFresults_IV = data.frame(Var = rep("", ncol(IVRegData_m)-1, 1), 
                           ADF = matrix(NaN, ncol(IVRegData_m)-1, 4))
colnames(ADFresults_IV)[-1] = c("stat_lev","stat_diff", "pval_lev","pval_diff")
for (i in 1:ncol(IVRegData_m)-1){
  ADF_level = ur.df(IVRegData_m[,i+1], type = "none")
  ADF_diff = ur.df(diff(na.omit(IVRegData_m[,i+1])), type = "none")
  
  ADFresults_IV[i,1] = colnames(IVRegData_m)[i+1]
  ADFresults_IV[i,2] = ADF_level@teststat
  ADFresults_IV[i,3] = ADF_diff@teststat
  ADFresults_IV[i,4] = ADF_level@testreg$coefficients[1,4]
  ADFresults_IV[i,5] = ADF_diff@testreg$coefficients[1,4]
}

# From the results tabulated in ADFresults_IV, log diff IIP is the only stationary
# variable. 

#taking first differences of non-stationary variable
IVData_m_stat = IVRegData_m[-1,]
ColstoDiff = !colnames(IVRegData_m) %in% c("Date","d_ln_IIP")
IVData_m_stat[ColstoDiff] = apply(IVRegData_m[ColstoDiff], 2, diff)


## First stage regressions -------------------------------------------------

### with 1st differencing ----------------------------------------------
Stage1 = lm(F_Own ~ 0 + EFFR + d_ln_IIP, data = IVData_m_stat )
summary(Stage1)

Stg1_F = summary(Stage1)$fstatistic["value"] 
tFCorr = 1.935 + (8.473 - Stg1_F)/(8.473 - 8.196)* (1.98-1.935)
# Calculation of tF se based on Stage 1 F-stat has to be manually done. Current 
# calculation is based on F-value of 8.39