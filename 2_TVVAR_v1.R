# This file has the analysis used to generate the figures from TVVAR used in the
# paper
# TVVAR uses 5 lags (p=5) and has 4 variables ("TIIE","MPTBA","GMXN30Y", "MXN_USD").
# Bootstrap uses empirical distribution (tboot = "wild").
# Robustness is done by changing 1mo to 6mo yield ("MPTBF") 
# Data used Mex_d_TV_diff is created in 2_ADFtest.R
VAR_1mo = CumIRF_1mo = CI_1mo = list()


start_time = Sys.time()
VAR_1mo = tvVAR(Mex_d_TV_diff[c("TIIE","MPTBA","GMXN30Y", "MXN_USD")],
                            p = 5, type = "none", bw = rep(0.1,4),tkernel = "Epa")
print(paste("VAR(5) estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CumIRF_1mo  = tvIRF(VAR_1mo, cumulative = T,
                       impulse =c("TIIE","MPTBA"), response = c("MPTBA","GMXN30Y"),
                       bw.cov = 0.1)
print(paste("IRF(5) estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CI_1mo = confint(CumIRF_1mo, level = 0.9, tboot = "wild")
print(paste("Confint for IRF(5) with wild errors estimated in",
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
beep(4)
    
  
  
