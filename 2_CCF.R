# 23Sep2024 - copied code not reviewed.
# code that generates CCFs of 10yr and 1mo. An alternate to the VAR analysis

# CCFs -------------------------------------------------------------------------

#generating CCFs on first difference for weekly data
ccf(diff(Mex2[,"r10y_i"]), diff(Mex2[,"r1mo_i"]),lag.max = 5,ylab = "CCF", xlab = "")
ccf(diff(Mex1[,"r10y_i"]), diff(Mex1[,"r1mo_i"]),lag.max = 5,ylab = "CCF", xlab = "")

#generating CCFs on first difference for daily data
ccf_d_h = ccf(diff(Mex2[,"r10y"]), diff(Mex2[,"r1mo"]),lag.max = 5,ylab = "CCF", xlab = "")
ccf_d_l = ccf(diff(Mex1[,"r10y"]), diff(Mex1[,"r1mo"]),lag.max = 5,ylab = "CCF", xlab = "")

