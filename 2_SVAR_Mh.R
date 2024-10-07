# This code uses Mex_w_d to run SVARs on a moving horizon

library(vars)

# Preparing subsamples ----------------------------------------------------

start = seq(as.Date("2006-01-01"),as.Date("2021-01-01"), by = '1 year' )
end = seq(as.Date("2007-12-31"),as.Date("2022-12-31"), by = '1 year' )

Vars_SVAR = list(r1mo = c("MPTBA","GMXN10Y","MXN_USD"),
                 rON = c("TIIE","GMXN10Y","MXN_USD"),
                 rON1mo = c("TIIE","MPTBA","MXN_USD"))

subsamp = list()

for (period in 1:length(start)){
  Index = Mex_w_d$Date >= start[period] & Mex_w_d$Date <= end[period]
  subsamp[[period]] = Mex_w_d[Index, Vars_SVAR$r1mo]
}


## Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initialising lists to store results
VARlags = list()     
VARs = list()
SVARs = list()
IRFs = list()
nonNA_periods = c()
for (period in 1:length(start)){
  if (!all(!is.na(subsamp[[period]])) ){
    
    print(paste('There are NAs in Period',period))
    
  }else{
    
    nonNA_periods = c(nonNA_periods, period)
    lagchoice = VARselect(subsamp[[period]], lag.max = 4, type = 'none')
    print(lagchoice$selection['AIC(n)'])                                            #The choice of lag according to AIC is always 1
    VARlags = c(VARlags, list(lagchoice) )
    
    samp_VAR = VAR(subsamp[[period]], type = "none")
    samp_SVAR = SVAR(samp_VAR,Bmat = b)
    samp_IRF = irf(samp_SVAR, impulse = 'MPTBA', n.ahead = 8,runs = 1000,
                   ci = 0.68)
    
    #Normalising IRFS to be responses to 1 unit shock
    samp_IRF$irf$MPTBA = samp_IRF$irf$MPTBA/samp_SVAR$B[1,'MPTBA']
    samp_IRF$Lower$MPTBA = samp_IRF$Lower$MPTBA/samp_SVAR$B[1,'MPTBA']
    samp_IRF$Upper$MPTBA = samp_IRF$Upper$MPTBA/samp_SVAR$B[1,'MPTBA']
    
    #storing results in lists
    VARs = c(VARs, list(samp_VAR) )
    SVARs = c(SVARs, list(samp_SVAR) )
    IRFs = c(IRFs, list(samp_IRF) )
  }}

names(VARs) = paste(substr(as.character(start[nonNA_periods]),1,4),
                    substr(as.character(end[nonNA_periods]),3,4), sep = "-")
names(SVARs) = names(VARs) 
names(IRFs) = names(VARs)

save(list = c(VARs, SVARs,IRFs,VARlags), file = 'VAR_MH_results.RData')

# Removing unnecessary variables ------------------------------------------

#rm(Index, Vars_SVAR, subsamp, samp_VAR, samp_SVAR, samp_IRF)



