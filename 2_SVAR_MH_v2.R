# This code uses Mex_w_d to run SVARs on different subsamples. The results are stored in
# 3 lists - VARs, SVARs and IRFs.
# Version 2 uses 10 year yields as well.

# To do (Sep 3) - create samp_dates such that it skips quarters and not 12 observations.
# To-Do: to avoid uncommon subsets, start_end should be adjusted to pick a date actually 
# in data that is the first after a specified date.Is Mex_w_d made with Friday dates ?

library(vars)


# Checking NA to understand range to analyzed -------------------------------

checkNACols = c("Date","TIIE","MPTBA","GMXN10Y","GMXN30Y")
checkNA = is.na(Mex_w_d[,checkNACols])
colMeans(checkNA)
for (i in 2:5){
  print(checkNACols[i])
print(Mex_w_d$Date[checkNA[,i]])
}

# TIIE available from 2006 until 2023 (all data), 1mo has missing from 2023 March
# onwards, 10Y has values missing in 2007 and 2008, and 30Y starts 2006 Nov onwards.

# Preparing date index for subsetting------------------------------------------
#I find the first date in Mex_w_d within 7 days of starting the year. Alternately,
# I could find Sunday date.
start_end = c(
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2008-04-01") & 
                 Mex_w_d$Date <= as.Date("2008-04-07")],
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2010-03-25") & 
                 Mex_w_d$Date <= as.Date("2010-03-31")])
start_end = which(Mex_w_d$Date %in% start_end) #finding index of the date. Index is easier to loop by
Mex_w_d$Date[start_end]
# A check of whether there are any NAs. There should be none as the range is selected based on that few lines ago
check = Mex_w_d[Mex_w_d$Date >= as.Date("2008-04-01") & 
                  Mex_w_d$Date <= as.Date("2022-12-31"), 
                c("Date", "TIIE","MPTBA","GMXN10Y","GMXN30Y", "MXN_USD") ]
colSums(is.na(check))

# Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initializing lists to store results
VARlags = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())   
VARs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
SVARs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
IRFs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
samp_dates = list() 

# Create a vector that stores names of first 2 SVAR variables for the 5 specifications run
SVARVars = c("TIIE","MPTBA","TIIE","GMXN10Y", "TIIE","GMXN30Y","MPTBA","GMXN10Y", "MPTBA","GMXN30Y") 

for (t in 1:56){ #56 is determined by just looping until samp_dates creation and seeing when the period ends
  D = start_end + ((t-1)*12)
  samp_dates = c(samp_dates, list(Mex_w_d$Date[D]) )

  for (Spec in 1:5){
    #Selecting appropriate subsample
    SVARVar = c(SVARVars[Spec*2 + c(-1,0)],"MXN_USD")
    samp = Mex_w_d[D[1]:D[2],SVARVar]
    
    lagchoice = VARselect(samp, lag.max = 4, type = 'none')
    print(paste("Start:", D[1],", Spec:", Spec, ", lags:",lagchoice$selection['AIC(n)']))                                            #The choice of lag according to AIC is always 1
    VARlags[[Spec]]  = c(VARlags[[Spec]] , list(lagchoice) )
    #VARlags[[`1mo_10y`]][[i]]$selection['AIC(n)']
    samp_VAR = VAR(samp, type = "none")
    samp_SVAR = SVAR(samp_VAR,Bmat = b)
    samp_IRF = irf(samp_SVAR, impulse = names(samp)[1], n.ahead = 8,runs = 1000,
                   ci = 0.68)
    
    #Normalising IRFS to be responses to 1 unit shock
    if (names(samp)[1] == 'MPTBA'){
      samp_IRF$irf$MPTBA = samp_IRF$irf$MPTBA/samp_SVAR$B[1,'MPTBA']
      samp_IRF$Lower$MPTBA = samp_IRF$Lower$MPTBA/samp_SVAR$B[1,'MPTBA']
      samp_IRF$Upper$MPTBA = samp_IRF$Upper$MPTBA/samp_SVAR$B[1,'MPTBA']
    }else if (names(samp)[1] == 'TIIE'){
      samp_IRF$irf$TIIE = samp_IRF$irf$TIIE/samp_SVAR$B[1,'TIIE']
      samp_IRF$Lower$TIIE = samp_IRF$Lower$TIIE/samp_SVAR$B[1,'TIIE']
      samp_IRF$Upper$TIIE = samp_IRF$Upper$TIIE/samp_SVAR$B[1,'TIIE']
    }
    
    #storing results in lists
    VARs[[Spec]] = c(VARs[[Spec]], list(samp_VAR) )
    SVARs[[Spec]]  = c(SVARs[[Spec]] , list(samp_SVAR) )
    IRFs[[Spec]]  = c(IRFs[[Spec]] , list(samp_IRF) )
    
    
  }} #end of time and spec loops


# for(i in 1:53){
#   print(VARlags[['ON_1mo']][[i]]$selection['AIC(n)'])
# }
save(samp_dates, VARlags,VARs,SVARs,IRFs, file = "SVARResultsMovHorzn_v2.RData" )

# Removing unnecessary variables ------------------------------------------

rm(checkNA,checkNACols,b,D,SVARVars, Vars_SVAR, samp, samp_VAR, samp_SVAR, samp_IRF)



