# This code uses Mex_w_d to run SVARs on different subsamples. The results are stored in
# 3 lists - VARs, SVARs and IRFs.

library(vars)

# Preparing date index for subsetting------------------------------------------
#I find the first date in Mex_w_d within 7 days of starting the year.
start_end = c(
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2010-01-01") & 
                 Mex_w_d$Date <= as.Date("2010-01-07")],
  Mex_w_d$Date[Mex_w_d$Date >= as.Date("2011-12-25") & 
                 Mex_w_d$Date <= as.Date("2011-12-31")])

check = Mex_w_d[Mex_w_d$Date >= as.Date("2010-01-01") & 
               Mex_w_d$Date <= as.Date("2016-01-17"), c("Date", "MPTBA","GMXN10Y", "MXN_USD") ]
colSums(is.na(check))

# Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initialising lists to store results
VARlags = list()   
VARs = list()
SVARs = list()
IRFs = list()
samp_dates = list()

for (i in 1:53){
  D = start_end + ((i-1)*28)
  samp_dates = c(samp_dates, list(D) )
  
  samp = Mex_w_d[which(Mex_w_d$Date == D[1]):which(Mex_w_d$Date == D[2]),
                 c("MPTBA","GMXN10Y", "MXN_USD")]
  
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
} #end of loop


# Removing unnecessary variables ------------------------------------------

rm(Index, Vars_SVAR, subsamp, samp_VAR, samp_SVAR, samp_IRF)



