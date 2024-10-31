
library(vars)



# Running Stage 1 regression ----------------------------------------------

start_end = c(as.Date("2006-01-01"), as.Date("2022-12-31"))

Stg1Data = EFFR_w[EFFR_w$Date >= start_end[1] & EFFR_w$Date <= start_end[2],]  
Stg1Data$F_Own_pcnt = Mex_w[Mex_w$Date >= start_end[1] & 
                            Mex_w$Date <= start_end[2], "F_Own_p"] *100        

#taking first differences
Stg1_stat = Stg1Data[-1,]
Stg1_stat[-1] = apply(Stg1Data[-1], MARGIN = 2, diff)

Stg1 = lm(F_Own_pcnt~ 0+  EFFR, data = Stg1_stat)
summary(Stg1)


# Running VARX ------------------------------------------------------------

sample = Mex_w_d[Mex_w_d$Date >= start_end[1] & Mex_w_d$Date <= start_end[2],
                 c("MPTBA","GMXN10Y","MXN_USD")]
sample = na.approx(sample)
VAR_result = VAR(sample,p=1, type = "none", exogen = Stg1$fitted.values)
summary(VAR_result)

IRF = irf(VAR_result, impulse = "exo1", n.ahead = 8)
rm(Stg1Data, Stg1_stat)