# This code uses Mex_w_d to run SVARs on different subsamples 

library(vars)

# Preparing subsamples ----------------------------------------------------

Index = cbind(
  Mex_w_d$Date <= as.Date("2011-12-31") & Mex_w_d$Date >= as.Date("2010-01-01"),
  Mex_w_d$Date <= as.Date("2013-12-31") & Mex_w_d$Date >= as.Date("2012-01-01"),
  Mex_w_d$Date <= as.Date("2015-12-31") & Mex_w_d$Date >= as.Date("2014-01-01"))

Vars_SVAR = list(
  rON6mo = c("TIIE","MPTBF","MXN_USD"),  rON01y = c("TIIE","GMXN01Y","MXN_USD"),
  rON02y = c("TIIE","GMXN02Y","MXN_USD"), rON05y = c("TIIE","GMXN05Y","MXN_USD"),
  rON10y = c("TIIE","GMXN10Y","MXN_USD"), rON30y = c("TIIE","GMXN30Y","MXN_USD"))

subsamp = list(
  sub_ON6mo = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON6mo],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON6mo],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON6mo]),
  sub_ON01y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON01y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON01y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON01y]),
  sub_ON02y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON02y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON02y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON02y]),
  sub_ON05y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON05y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON05y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON05y]),
  sub_ON10y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON10y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON10y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON10y]),
  sub_ON30y = list(s1 = Mex_w_d[Index[,1], Vars_SVAR$rON30y],
                    s2 = Mex_w_d[Index[,2], Vars_SVAR$rON30y],
                    s3 = Mex_w_d[Index[,3], Vars_SVAR$rON30y])
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
      samp_IRF = irf(samp_SVAR, impulse = names(samp)[1], n.ahead = 6,runs = 1000,
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



