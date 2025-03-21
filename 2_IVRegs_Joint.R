# This file uses IVRegData_m created by 2_IVRegs_Mat_Flow.R 

# 1. 2SLS regression of bid-ask spreads (in bps) on value foreign owned bonds (in millions of Pesos)
# 2. Using 1st stage fitted values as X in ARIMAX model on yields

# packages needed - ivreg, forecast, ur.ca - already loaded by 2_IVRegs_Mat_Flow.R 
library(lmtest) #for Wald test
library(marginaleffects)
library(stargazer)
Yield_long = melt(IVData_m_stat[,c(1,19,20,23,22,6:18)], id.vars = "Date") #selecting yields except 1 week in order of maturity

# Testing Coefficient of each Yield --------------------------------------------

# I need to make a choice of which model to fit all yields. In the individual
# regressions, half were ARIMAX(0,1,0), and half (1,1,0). 
IVData_Long = cbind(Yield_long,
                    fit = kronecker(diag(17), Stage1$fitted.values),
                    TIIE = kronecker(diag(17), IVData_m_stat[,"TIIE"]))

# renaming columns so that the last two letters show which yield the X is regressing.
# Renaming needed only for monthly and 15,20,30 yr yields.
for (name in c("fit.", "TIIE.")){ 
  colnames(IVData_Long)[colnames(IVData_Long) %in% paste(name,1:4, sep = "")] =
    paste(name,c(1,3,6,9), "m", sep = "")
  colnames(IVData_Long)[colnames(IVData_Long) %in% paste(name,15:17, sep = "")] =
    paste(name,c(15,20,30), sep = "")
}


# Running ARIMA(0,1,0) on all yields
formula = paste("value ~ 0", paste(" + ",colnames(IVData_Long)[4:37],
                                   sep = "",collapse  = ""))
IVLong_result = lm(formula, data = IVData_Long)
summary(IVLong_result)
hypotheses(IVLong_result, hypothesis = "fit.30 - fit.1m = 0")
hypotheses(IVLong_result, hypothesis = "fit.3m - fit.1m = 0")
linearHypothesis(IVLong_result)
# Running ARIMA(1,1,0) on all yields
IVLong_result2 = Arima(IVData_Long[,"value"], order = c(1,0,0),
                 xreg = data.matrix(IVData_Long[,4:37]), include.mean = F)
summary(IVLong_result2)
(1-pnorm(abs(IVLong_result2$coef)/sqrt(diag(IVLong_result2$var.coef))))*2
#hypotheses(summary(IVLong_result2), hypothesis = "fit.30 - fit.1m = 0") hypotheses will not work with Arima

# Testing Coefficient on short vs medium vs long--------------------------------

## Creating Data

N = c(5,7,5) # defining number of short, medium and long yields
T = length(Stage1$fitted.values)

IVData_Long1 = cbind(Yield_long,
                     fit_S = c(rep(Stage1$fitted.values,N[1]), rep(0, (N[2] + N[3])*T ) ),
                     fit_M = c(rep(0, N[1]*T), rep(Stage1$fitted.values,N[2]), rep(0, N[3]*T ) ),
                     fit_L =c(rep(0, (N[1] + N[2])*T ),rep(Stage1$fitted.values,N[3]) ),
                     TIIE_S = c(rep(IVData_m_stat[,"TIIE"],N[1]), rep(0, (N[2] + N[3])*T ) ),
                     TIIE_M = c(rep(0, N[1]*T), rep(IVData_m_stat[,"TIIE"],N[2]), rep(0, N[3]*T ) ),
                     TIIE_L = c(rep(0, (N[1] + N[2])*T ),rep(IVData_m_stat[,"TIIE"],N[3]) ) )

## Running Regression and testing hypothesis

IVLong1_result = lm("value ~ 0 + fit_S + fit_M + fit_L + TIIE_S + TIIE_M + TIIE_L",
                   data = IVData_Long1)
summary(IVLong1_result)
#tFCorr = 1.935 + (8.473 - Stg1_F)/(8.473 - 8.196)* (1.98-1.935)
#IVLong1_result$
#stargazer(IVLong1_result)
#hypotheses(IVLong1_result, hypothesis = "fit_S - fit_L = 0")                     
                    
#pdq010 = AutoAR_results[,2] == 0 & AutoAR_results[,3] == 1 & AutoAR_results[,4] ==0
