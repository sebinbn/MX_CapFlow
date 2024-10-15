# This file uses list Stage1 created by 2_EFFR_IV.R to then use fitted values in
# running VARX

library(vars)


Index = cbind(
  Mex_w_d$Date <= as.Date("2016-12-31") & Mex_w_d$Date >= as.Date("2010-01-01"))
sample = Mex_w_d[Index, c("MPTBA","GMXN10Y","MXN_USD")]

VAR_result = VAR(sample, type = "none")
