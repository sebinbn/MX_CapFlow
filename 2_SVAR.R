# This code uses Mex_w_d to run SVARs on different subsamples 

library(vars)

# Preparing subsamples ----------------------------------------------------

Index = cbind(
  Mex_w_d$Date <= as.Date("2011-12-31") & Mex_w_d$Date >= as.Date("2010-01-01"),
  Mex_w_d$Date <= as.Date("2013-12-31") & Mex_w_d$Date >= as.Date("2012-01-01"),
  Mex_w_d$Date <= as.Date("2015-12-31") & Mex_w_d$Date >= as.Date("2014-01-01"))
  
Vars_SVAR = list(
  r1mo6mo = c("MPTBA","MPTBF","MXN_USD"),  r1mo01y = c("MPTBA","GMXN01Y","MXN_USD"),
  r1mo02y = c("MPTBA","GMXN02Y","MXN_USD"), r1mo05y = c("MPTBA","GMXN05Y","MXN_USD"),
  r1mo10y = c("MPTBA","GMXN10Y","MXN_USD"), r1mo30y = c("MPTBA","GMXN30Y","MXN_USD"),
  rON = c("TIIE","GMXN10Y","MXN_USD"), rON1mo = c("TIIE","MPTBA","MXN_USD"))

subsamp = list(
  sub_1mo6mo = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo6mo],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo6mo],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo6mo]),
  sub_1mo01y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo01y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo01y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo01y]),
  sub_1mo02y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo02y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo02y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo02y]),
  sub_1mo05y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo05y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo05y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo05y]),
  sub_1mo10y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo10y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo10y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo10y]),
  sub_1mo30y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$r1mo30y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$r1mo30y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$r1mo30y]),
  sub_ON = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON],
                s2 = Mex_w_d[Index[,2], Vars_SVAR$rON],
                s3 = Mex_w_d[Index[,3], Vars_SVAR$rON]),
  sub_ON1mo = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON1mo],
                   s2 = Mex_w_d[Index[,2], Vars_SVAR$rON1mo],
                   s3 = Mex_w_d[Index[,3], Vars_SVAR$rON1mo])
)
                

# Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initialising lists to store results
VARlags = list()   
VARs = list()
SVARs = list()
IRFs = list()
i = 0
for (sub_ in subsamp){
  i = i + 1
  j = 0
  for (samp in sub_){
    j = j + 1
    if (!all(!is.na(samp)) ){
      
      print(paste('There are NAs in sample',names(subsamp)[i], names(sub_)[j] ))
      VARs = c(VARs, list(NaN) )
      SVARs = c(SVARs, list(NaN) )
      IRFs = c(IRFs, list(NaN) )
      
    }else{
    
      lagchoice = VARselect(samp, lag.max = 4, type = 'none')
      print(lagchoice$selection['AIC(n)'])                                            #The choice of lag according to AIC is always 1
      VARlags = c(VARlags, list(lagchoice) )
      
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
      VARs = c(VARs, list(samp_VAR) )
      SVARs = c(SVARs, list(samp_SVAR) )
      IRFs = c(IRFs, list(samp_IRF) )
    }
  } #end of looping across periods
} #end of looping across SVAR variables


# Removing unnecessary variables ------------------------------------------

rm(Index, Vars_SVAR, subsamp, samp_VAR, samp_SVAR, samp_IRF)



