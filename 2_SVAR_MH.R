# This code uses Mex_w_d to run SVARs on different subsamples. The results are stored in
# 3 lists - VARs, SVARs and IRFs.

library(vars)


# Checking NA to understand range to analyzed -------------------------------

checkNA = is.na(Mex_w_d[,c("Date","TIIE","MPTBA","GMXN10Y")])
Mex_w_d$Date[checkNA[,2]]

# TIIE available from 2006 until 2023 (all data), 1 month missing from 2023 March
# and has 4 missing values )
# Preparing date index for subsetting------------------------------------------
#I find the first date in Mex_w_d within 7 days of starting the year. Alternately,
# I could find Sunday date.
start_end = c(
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2009-01-01") & 
                 Mex_w_d$Date <= as.Date("2009-01-07")],
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2010-12-25") & 
                 Mex_w_d$Date <= as.Date("2010-12-31")])
start_end = which(Mex_w_d$Date %in% start_end) #finding index of the date. Index is easier to loop by
Mex_w_d$Date[start_end]
# A check of whether there are any NAs. There should
check = Mex_w_d[Mex_w_d$Date >= as.Date("2009-01-01") & 
               Mex_w_d$Date <= as.Date("2022-12-31"), 
               c("Date", "TIIE","MPTBA","GMXN10Y", "MXN_USD") ]
colSums(is.na(check))

# Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initializing lists to store results
VARlags = list(ON_1mo = list(), ON_10y = list(), `1mo_10y` = list())   
VARs = list(ON_1mo = list(), ON_10y = list(), `1mo_10y` = list()) 
SVARs = list(ON_1mo = list(), ON_10y = list(), `1mo_10y` = list()) 
IRFs = list(ON_1mo = list(), ON_10y = list(), `1mo_10y` = list()) 
samp_dates = list() 

SVARVars = c("TIIE","MPTBA","TIIE","GMXN10Y","MPTBA","GMXN10Y") #a vector that stores names of first 2 SVAR variables for the 3 specifications run


for (t in 1:53){
  D = start_end + ((t-1)*12)
  samp_dates = c(samp_dates, list(Mex_w_d$Date[D]) )
 
  for (Spec in 1:3){
    #Selecting appropriate subsample
    SVARVar = c(SVARVars[Spec*2 + c(-1,0)],"MXN_USD")
    samp = Mex_w_d[D[1]:D[2],SVARVar]
                
      lagchoice = VARselect(samp, lag.max = 4, type = 'none')
      print(lagchoice$selection['AIC(n)'])                                            #The choice of lag according to AIC is always 1
      VARlags[[Spec]]  = c(VARlags[[Spec]] , list(lagchoice) )
      
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


# Removing unnecessary variables ------------------------------------------

rm(Index, Vars_SVAR, subsamp, samp_VAR, samp_SVAR, samp_IRF)



