# This code runs time varying VAR on daily data
# It runs ADF test to check stationarity, then first differences it.

library(urca)
library(vars)

# Creating DataTable for Regression ---------------------------------------


# Daily data is available in Mex_d for all variables.

# I first scroll through the daily data to find where are a lot of continuous NAs,
# and subset the data to avoid these.
# GMXN10Y is missing in 2007 and upto 15-02-2008. Subset data to start from March 2008.
# MPTBA only available until 2023 MArch. subset upto 31-12-2022.

# A series of weekdays are created and then populated. To get a VAR lag order that is meaningful,
# say 5 for 5 weekdays, it is better not to remove NAs that also would remove market holidays.
# Removing NAs was tried initially, but decided to go with former method as VARsekect showed 5 for
# most combinations, but higher lags for some other 3 variable combinations.

all_days <- seq.Date(from = as.Date("2008-03-01"), to = as.Date("2022-12-31"),
                     by = "day")
x <- data.frame(Date = all_days[!(weekdays(all_days) %in% c("Saturday", "Sunday"))] )
Mex_d_TV = merge(x, Mex_d, by.x = "Date", all.x = T)

Vars_ADF = c("MPTBA", "GMXN10Y","GMXN30Y", "TIIE", "MXN_USD", "F_Own", "F_Own_p")
Mex_d_TV[Vars_ADF] = na.approx(Mex_d_TV[Vars_ADF], na.rm = F)

# following code is modified and run thrice for each of the variables
colSums(apply(Mex_d_TV,2, FUN = is.na))

# #Now, remove rows where all three interest rates are missing. These are presumably market holidays.
# Mex_d_TV = Mex_d_TV[!(is.na(Mex_d_TV$GMXN10Y) & is.na(Mex_d_TV$GMXN30Y) & 
#                         is.na(Mex_d_TV$MPTBA)),]
# 
# x = Mex_d_TV[is.na(Mex_d_TV$GMXN10Y),c("Date","MPTBA", "GMXN10Y","GMXN30Y")]
# weekdays(x$Date) #these are not weekends for any of the 3
# rm(x)
# #Now there are 12 GMXN10Y and GMXN30Y missing and 65 MPTBA missing.None of these
# # are weekends. Of the 12, 11 are the same days when both 10 and 30 are missing. 
# # So, dates where GMXN10Y and GMNX30Y are missing are dropped. The remaining MPTBA
# # missing are filled in with linear interpolation.
# 
# Mex_d_TV = Mex_d_TV[!(is.na(Mex_d_TV$GMXN10Y) | is.na(Mex_d_TV$GMXN30Y)),]
# Mex_d_TV$MPTBA = na.approx(Mex_d_TV$MPTBA,na.rm = F)


# Running ADF Test --------------------------------------------------------

Mex_d_TV_diff = lapply(Mex_d_TV[,Vars_ADF ], diff)                            #lapply has to be used because diff() does not work on dartaframes
Mex_d_TV_diff  = data.frame(Date = Mex_d_TV$Date[-1], Mex_d_TV_diff)

colSums(apply(Mex_d_TV_diff,2, FUN = is.na))
ADF_result_l = apply(Mex_d_TV[Vars_ADF], 2, FUN = ur.df, type = "none",
                     selectlags = "AIC" )
ADF_result_d = apply(Mex_d_TV_diff[Vars_ADF], 2, FUN = ur.df, type = "none",
                     selectlags = "AIC" )

for (i in 1:length(Vars_ADF)){
  x = summary(ADF_result_l[[i]])@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
  print(paste("Levels - ", Vars_ADF[i], ". t-stat - ", round(x[1],3),
              "p-val - ", round(x[2],3) ))
  x = summary(ADF_result_d[[i]])@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
  print(paste("Diff - ", Vars_ADF[i], ". t-stat - ", round(x[1],3),
              "p-val - ", round(x[2],3) ))
} #shows that all variables have unit root and are stationary at first differences.



# Running TVVAR against time ---------------------------------------------------

#Initializing lists to store results
VARlags = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), 
               `1mo_10y` = list(), `1mo_30y` = list(), ON_1mo_30y = list())  
VARs = VARs_z = CumIRFs = CumIRFs_z = Regs = ARIMAs = ARIMA_pVals = VARlags


VARSpec = list(c("TIIE","MPTBA","MXN_USD"),
               c("TIIE","GMXN10Y","MXN_USD"),
               c("TIIE","GMXN30Y","MXN_USD"),
               c("MPTBA","GMXN10Y","MXN_USD"),
               c("MPTBA","GMXN30Y","MXN_USD"),
               c("TIIE","MPTBA","GMXN30Y","MXN_USD"))

for(Spec in 6:6){
  # VARlags[[Spec]] = VARselect(Mex_d_TV_diff[VARSpec[[Spec]]], lag.max = 7, type = 'none')
  # print(c(VARSpec[[Spec]],
  #        paste(", lags:",VARlags[[Spec]]$selection['AIC(n)']) ))
  #  VARs[[Spec]] = VAR(Mex_w_TV[VARSpec[[Spec]]], p =5, type = "none")
  # print(summary(VARs[[Spec]]))
  # VARs[[Spec]] = tvVAR(Mex_d_TV_diff[VARSpec[[Spec]]], p =1, type = "none",
  #                      tkernel = "Epa", est = "lc", bw = c(20,20,20,20))

  # VARs_z[[Spec]] = tvVAR(Mex_w_TV[VARSpec[[Spec]]], p =1, type = "none", 
  #                      z = Mex_w_TV$F_Own_p )
  # 
  CumIRFs[[Spec]] = tvIRF(VARs[[Spec]], impulse = VARSpec[[Spec]][1:2],
                          response = VARSpec[[Spec]][2:3], cumulative = T)
  # CumIRFs_z[[Spec]] = tvIRF(VARs_z[[Spec]], impulse = VARSpec[[Spec]][1],
  #                         response = VARSpec[[Spec]][2], cumulative = T)
  
  # if(Spec<4){
   Regs[[Spec]] = lm(CumIRFs[[Spec]]$irf$TIIE[,1,11]~ Mex_d_TV$F_Own_p[-c(1,2)])
   Regs[[Spec]] = lm(CumIRFs[[Spec]]$irf$MPTBA[,2,11]~ Mex_d_TV$F_Own_p[-c(1,2)])
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
VARs$ON_1mo_30y$coefficients$TIIE
length(CumIRFs[[Spec]]$irf$TIIE[,1,11])
length(Mex_d_TV$F_Own_p)
plot( Mex_d_TV$F_Own[-c(1,2)],CumIRFs[[Spec]]$irf$TIIE[,1,11])
plot( Mex_d_TV$F_Own_p[-c(1,2)],CumIRFs[[Spec]]$irf$MPTBA[,2,11])
plot(CumIRFs[[Spec]]$irf$TIIE[,1,11])
plot(CumIRFs[[Spec]]$irf$MPTBA[,2,11])

IRFResults = data.frame(Date = Mex_d_TV_diff$Date[-1], 
           ON_1mo = CumIRFs[[Spec]]$irf$TIIE[,1,11],
           mo1_30Y = CumIRFs[[Spec]]$irf$MPTBA[,2,11],
           F_Own_p = Mex_d_TV$F_Own_p[-c(1,2)] - 0.2) #rescaling F_Own_p to fit in the same graph


IRFPlotData = melt(IRFResults[-2], id.vars = "Date", variable.name = "Spec")

Plot_cumIRF = ggplot(IRFPlotData, aes(x = Date, y = value, color = Spec)) +
  geom_line(linewidth = 1.2) +
   scale_y_continuous(name = 'Cumulative Transmission (in pp.)',
                      sec.axis = sec_axis(transform=~.+0.2, name="Prop. bonds foreign owned"))+
   scale_color_discrete(labels = c("ON_1mo" = "10 day Response of 1 month yield",
                                   "mo1_30Y" = "10 day Response of 30yr yield",
                                   "F_Own_p" = "Prop. bonds Foreign owned")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  # labs(x = element_blank(), title = 'Imp = ON, Resp = 1mo') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), legend.position = "bottom")
Plot_cumIRF

ARIMA_pVals[[5]]
summary(Regs[[5]])
save(VARs,CumIRFs, file = "TVVARResults.RData" )
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