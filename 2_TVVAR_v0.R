# this version is the first pass at running TVVARs using weekly data. The analysis
# used in the paper is done in 2_TVVAR_v1

# install.packages('tvReg')


# Creating DataTable for Regression ---------------------------------------


# First differenced weekly data is available for yields and exchange rate in 
# Mex_w_d. Daily frequency foreign ownership data is in Mex_FO. Weekly data is
# created for Mex_FO and merged with Mex_w_d to finally output Mex_w_TV

Mex_w_TV = Mex_w_d[Mex_w_d$Date >= as.Date("2008-04-01") &
                     Mex_w_d$Date <= as.Date("2023-01-01"),]    # 2023-01-01 is a Sunday

Sun  = seq(as.Date("2008-04-06"), as.Date("2023-01-01"), by = "7 days")         #creating vector of Sundays. 2008-04-06 was Sunday.

Mex_FO_w = data.frame(Date = Sun, Values = matrix(NaN, length(Sun),ncol =2) )    # intitiating a dataframe           
colnames(Mex_FO_w)[-1] = colnames(Mex_FO)[-1]
                                    

for(i in 1:length(Sun)){                                                          
  Week_Data = Mex_FO[Mex_FO$Date <= Sun[i] & Mex_FO$Date > (Sun[i]-7),]
  
  for (V in names(Mex_FO)[-1]){
    a = na.omit(Week_Data[V])
    Mex_FO_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
}

Mex_w_TV = merge(Mex_w_TV, Mex_FO_w, by = "Date")
Mex_w_TV$F_Own_p = Mex_w_TV$F_Own_p*10

# Running TVVAR against time ---------------------------------------------------

#Initializing lists to store results
# VARlags = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())   
VARs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
VARs_z = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
CumIRFs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
CumIRFs_z = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
Regs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
ARIMAs = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
ARIMA_pVals =  list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())
lagchoice =  list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), `1mo_10y` = list(), `1mo_30y` = list())


VARSpec = list(c("TIIE","MPTBA","MXN_USD"),
               c("TIIE","GMXN10Y","MXN_USD"),
               c("TIIE","GMXN30Y","MXN_USD"),
               c("MPTBA","GMXN10Y","MXN_USD"),
               c("MPTBA","GMXN30Y","MXN_USD") )
   
for(Spec in 1:5){
  lagchoice[[Spec]] = VARselect(Mex_w_TV[VARSpec[[Spec]]], lag.max = 4, type = 'none')
  print(c(VARSpec[[Spec]],
          paste(", lags:",lagchoice[[Spec]]$selection['AIC(n)']) ))
  VARs[[Spec]] = tvVAR(Mex_w_TV[VARSpec[[Spec]]], p =4, type = "none") 
  #                     
  # VARs_z[[Spec]] = tvVAR(Mex_w_TV[VARSpec[[Spec]]], p =1, type = "none", 
  #                      z = Mex_w_TV$F_Own_p )
  # 
  CumIRFs[[Spec]] = tvIRF(VARs[[Spec]], impulse = VARSpec[[Spec]][1],
                         response = VARSpec[[Spec]][2], cumulative = T)
  # CumIRFs_z[[Spec]] = tvIRF(VARs_z[[Spec]], impulse = VARSpec[[Spec]][1],
  #                         response = VARSpec[[Spec]][2], cumulative = T)

  # if(Spec<4){
  #   Regs[[Spec]] = lm(CumIRFs[[Spec]]$irf$TIIE[,,11]~ Mex_w_TV$F_Own_p[-1])
  #   ARIMAs[[Spec]] = Arima(CumIRFs[[Spec]]$irf$TIIE[,,11], order = c(1,0,0), 
  #                          xreg = diff(Mex_w_TV$F_Own_p))
  # } else{
  #    Regs[[Spec]] = lm(CumIRFs[[Spec]]$irf$MPTBA[,,11]~ Mex_w_TV$F_Own_p[-1])
  #    ARIMAs[[Spec]] = Arima(CumIRFs[[Spec]]$irf$MPTBA[,,11], order = c(1,0,0), 
  #                        xreg = diff(Mex_w_TV$F_Own_p))
  #  }
  # ARIMA_pVals[[Spec]] = 
  #   (1-pnorm(abs(ARIMAs[[Spec]]$coef)/sqrt(diag(ARIMAs[[Spec]]$var.coef))))*2
  
  # print(summary(Regs[[Spec]]))
  # print(summary(ARIMAs[[Spec]]))
}
ARIMA_pVals[[5]]
summary(Regs[[5]])

summary(CumIRFs[[5]])
summary(CumIRFs_z[[5]])
plot(CumIRFs_z[[1]], obs.index = 760)

# Regressions on cumIRFs --------------------------------------------------

regON1mo = lm(IRF1$irf$TIIE[,,11]~ Mex_w_TV$F_Own_p[-1])
summary(regON1mo)


VAR1 = tvVAR(Mex_w_TV[VARSpec[[1]]], p =1, type = "none" )
IRF1 = tvIRF(VAR1, impulse = "TIIE", response = "MPTBA", runs = 100, level = 0.68,
             cumulative = T)
IRF2 = tvIRF(VAR1, impulse = "TIIE", response = "MPTBA", runs = 100, level = 0.68)
plot(IRF1)
View(tvIRF)
IRF1$irf$TIIE[,,11]