# This code uses Mex_w_d to run SVARs on different subsamples. The results are stored in
# 3 lists - VARs, SVARs and IRFs. Since there are 17 different combinations of 
# variables and 3 periods, there are a total of 51 results in each list.

library(vars)

# Preparing date index for subsetting------------------------------------------

Index = cbind(
  Mex_w_d$Date <= as.Date("2011-12-31") & Mex_w_d$Date >= as.Date("2010-01-01"),
  Mex_w_d$Date <= as.Date("2013-12-31") & Mex_w_d$Date >= as.Date("2012-01-01"),
  Mex_w_d$Date <= as.Date("2015-12-31") & Mex_w_d$Date >= as.Date("2014-01-01"))

Vars_NoSVAR = c("Date","MPTB1", "F_Own", "F_Own_p","Tgt_rate", "TIIE", "MXN_USD")
Vars_SVAR = names(Mex_w)[!names(Mex_w) %in% Vars_NoSVAR]

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
for (Var2 in Vars_SVAR){
  Var_SVAR = c("TIIE", Var2,"MXN_USD")
  sub_ = list(s1 = Mex_w_d[Index[,1], Var_SVAR],
                     s2 = Mex_w_d[Index[,2], Var_SVAR],
                     s3 = Mex_w_d[Index[,3], Var_SVAR])
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



