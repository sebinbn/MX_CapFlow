
library(vars)


Index = Mex_w_d$Date <= as.Date("2016-12-31") & 
  Mex_w_d$Date >= as.Date("2010-01-01")
sample = Mex_w_d[Index, c("MPTBA","GMXN10Y","MXN_USD")]

Stg1Data = EFFR_w[EFFR_w$Date <= as.Date("2016-12-31") & 
                    EFFR_w$Date >= as.Date("2009-12-27"),]  #2009-12-27 chosen so that there is a value to difference
Stg1Data$F_Own_pcnt = Mex_w[Mex_w$Date <= as.Date("2016-12-31") & 
                            Mex_w$Date >= as.Date("2009-12-27"), "F_Own_p"] *100        

#taking first differences
Stg1_stat = Stg1Data[-1,]
Stg1_stat[-1] = apply(Stg1Data[-1], MARGIN = 2, diff)

Stg1 = lm(F_Own_pcnt~ 0 + EFFR, data = Stg1_stat)
summary(Stg1)
fitx = Stg1$fitted.values
VAR_result = VAR(sample,p=1, type = "none", exogen = Stg1$fitted.values)
summary(VAR_result)

rm(Stg1Data, Stg1_stat)