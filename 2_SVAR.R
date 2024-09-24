# This code uses Mex_w_d to run SVARs on different subsamples 

library(vars)

# Preparing subsamples ----------------------------------------------------

Index = cbind(
  Mex_w_d$Date <= as.Date("2011-12-31") & Mex_w_d$Date >= as.Date("2010-01-01"),
  Mex_w_d$Date <= as.Date("2013-12-31") & Mex_w_d$Date >= as.Date("2012-01-01"),
  Mex_w_d$Date <= as.Date("2015-12-31") & Mex_w_d$Date >= as.Date("2014-01-01"))
  
Vars_SVAR = list(r1mo = c("MPTBA","GMXN10Y","MXN_USD"),
                 rON = c("TIIE","GMXN10Y","MXN_USD"))

subsamp = list( s1_1mo = Mex_w_d[Index[,1], Vars_SVAR$r1mo], 
                s2_1mo = Mex_w_d[Index[,2], Vars_SVAR$r1mo],
                s3_1mo = Mex_w_d[Index[,3], Vars_SVAR$r1mo],
                s1_ON = Mex_w_d[Index[,1], Vars_SVAR$rON], 
                s2_ON = Mex_w_d[Index[,2], Vars_SVAR$rON],
                s3_ON = Mex_w_d[Index[,3], Vars_SVAR$rON])
                


# Estimating VAR and SVAR --------------------------------------------------

## Identifying ideal VAR lag length ----------------------------------------

VARlags = list()                                                                  #Initialising  list to store results

for (samp in subsamp){
  lagchoice = VARselect(samp1, lag.max = 4, type = 'none')
  print(lagchoice$selection['AIC(n)'])                                            #The choice of lag according to AIC is always 1
  VARlags = c(VARlags, list(lagchoice) )
}


## Estimating VAR,SVAR,IRF -------------------------------------------------

# creating matrix that imposes short run restrictions
b = matrix(NA,nrow = 3, ncol = 3)
b[upper.tri(b)] = 0

#Initialising lists to store results
VARs = list()
SVARs = list()
IRFs = list()

for (samp in subsamp){
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


# Plotting IRFs -----------------------------------------------------------

## Comparing 2010-11 with 2014-15 with 1 month yield -----------------------

irfDat_1011 = data.frame(Week = 0:(nrow(IRFs[[1]]$irf$MPTBA) - 1),
                      IRF = IRFs[[1]]$irf$MPTBA[,'GMXN10Y'],
                      Upper_CI = IRFs[[1]]$Upper$MPTBA[,'GMXN10Y'],
                      Lower_CI = IRFs[[1]]$Lower$MPTBA[,'GMXN10Y'])

IRF_1011 = ggplot(irfDat_1011, aes(x = Week)) +
  geom_line(aes(y = IRF), color = "black", linewidth = 1.2) +  # Main IRF line
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = 'blue', alpha = 0.3) +  # CI band
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Zero line
  labs(title = "SVAR Impulse Response from TIIE", y = "Response", x = "Week") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))






