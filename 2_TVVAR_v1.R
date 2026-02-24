# This file has the analysis used to generate the figures from TVVAR used in the
# paper
# TVVAR uses 5 lags (p=5) and has 4 variables ("TIIE","MPTBA","GMXN30Y", "MXN_USD").
# Bootstrap uses empirical distribution (tboot = "wild").
# Robustness is done by changing 1mo to 6mo yield ("MPTBF") 
# Data used Mex_d_TV_diff is created in 2_ADFtest.R
# This has dependency on a local function SN_tvIRF() that is created in A_SN_tvIRF.R

VAR_1mo = CumIRF_1mo = CI_1mo = list()
VAR_vars = c("TIIE","MPTBA","GMXN30Y", "MXN_USD")

start_time = Sys.time()
VAR_1mo = tvVAR(Mex_d_TV_diff[VAR_vars],p = 5, type = "none", tkernel = "Epa",
                z= Mex_d_TV$F_Own_p[-1], bw = rep(0.05,4))
print(paste("VAR(5) estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CumIRF_1mo_bw  = SN_tvIRF(VAR_1mo, cumulative = T, bw.cov = 0.05, unit.shock = T,
                       impulse =VAR_vars, response = VAR_vars)
print(paste("IRF(5) estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CI_1mo_1 = SN_CI_tvirf(CumIRF_1mo, level = 0.68, tboot = "wild")
print(paste("Confint for IRF(5) with wild errors estimated in",
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
beep(4)

xvar = Mex_d_TV_diff$F_Own_p[-(1:6)]*10
summary(lm(diff(CumIRF_1mo_bw$irf$TIIE[,"MPTBA",11]) ~ xvar ))

# Running analysis on 6 month ---------------------------------------------

AR_6mo = CumIRF_6mo = CI_6mo = list()


start_time = Sys.time()
VAR_6mo = tvVAR(Mex_d_TV_diff[c("TIIE","MPTBF","GMXN30Y", "MXN_USD")],
                p = 5, type = "none", bw = rep(0.1,4),tkernel = "Epa")
print(paste("VAR(5) estimated in", 
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CumIRF_6mo  = tvIRF(VAR_6mo, cumulative = T,
                    impulse =c("TIIE","MPTBF"), response = c("MPTBF","GMXN30Y"),
                    bw.cov = 0.1)
print(paste("IRF(5) estimated in", 
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CI_6mo = confint(CumIRF_6mo, level = 0.9, tboot = "wild")
print(paste("Confint for IRF(5) with wild errors estimated in",
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
beep(4)
    
  
  save(VAR_1mo,VAR_6mo,CumIRF_1mo,CumIRF_6mo,CI_1mo,CI_6mo, 
       file = "SavedRResults/tvVARResults_Oct17.RData")
