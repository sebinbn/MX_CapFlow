
#  Estimate time varying VAR and IRFs and confidence intervals for
# 1. Three variable VAR with 
#  1.1 lag choice 5 and lag choice 1
#  1.2 Use wild and wild2 bootstrap

# Running TVVAR indexed by FO ---------------------------------------------------

VAR3_MPTBA = CumIRF3_MPTBA = list(p5 = list(), p1 = list())
CI3_MPTBA = list(p5B2 = list(), p5B1 = list(),p1B2 = list(), p1B1 = list())

VAR3_MPTBF = CumIRF3_MPTBF = list(p5 = list(), p1 = list())
CI3_MPTBF = list(p5B2 = list(), p5B1 = list(),p1B2 = list(), p1B1 = list())

loc = 1
for (p in c(1,5)){
  start_time = Sys.time()
  VAR3_MPTBF[[loc]] = tvVAR(Mex_d_TV_diff[c("TIIE","MPTBF","MXN_USD")], p = p,
                              type = "none", bw = rep(0.1,4),tkernel = "Epa")
  print(paste("VAR(",p,") estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
  start_time = Sys.time()
  result_CIRF  = tvIRF(VAR3_z_MPTBF[[loc]], cumulative = T,
                                 impulse ="TIIE",response = "MPTBF",
                                 bw.cov = 0.1)
  
  CumIRF3_MPTBF[[loc]] = result_CIRF
  print(paste("IRF(",p,") estimated in",
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
  start_time = Sys.time()
  B_loc = loc * 2 - 1
  loc = loc +1
  for (B in c("wild","wild2")){
    CI3_MPTBF[[B_loc]] = confint(result_CIRF, level = 0.9, tboot = B)
    print(paste("Confint for IRF(",p,") with",B,"errors estimated in",
                round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
    start_time = Sys.time()
    B_loc = B_loc + 1
  }
  
}


# Running 4 variable SVAR -------------------------------------------------

VAR4_MPTBA = CumIRF4_MPTBA = list(p5 = list(), p1 = list())
CI4_MPTBA = list(p5B2 = list(), p5B1 = list(),p1B2 = list(), p1B1 = list())

loc = 1
for (p in c(1,5)){
  start_time = Sys.time()
  VAR4_MPTBA[[loc]] = tvVAR(Mex_d_TV_diff[c("TIIE","MPTBA","GMXN30Y", "MXN_USD")],
                              p = p, type = "none", bw = rep(100,4),tkernel = "Epa")
  print(paste("VAR(",p,") estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
  start_time = Sys.time()
  result_CIRF  = tvIRF(VAR4_MPTBA[[loc]], cumulative = T,
                       impulse =c("TIIE","MPTBA"), response = c("MPTBA","GMXN30Y"),
                       bw.cov = 100)
  
  CumIRF4_MPTBA[[loc]] = result_CIRF
  print(paste("IRF(",p,") estimated in", 
              round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
  start_time = Sys.time()
  B_loc = loc * 2 - 1
  loc = loc +1
  for (B in c("wild","wild2")){
    CI4_MPTBF[[B_loc]] = confint(result_CIRF, level = 0.9, tboot = B)
    print(paste("Confint for IRF(",p,") with",B,"errors estimated in",
                round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
    start_time = Sys.time()
    B_loc = B_loc + 1
  }
  
}
save(VAR3_z_MPTBA,VAR3_z_MPTBF, VAR4_z_MPTBA, VAR4_z_MPTBF,
     CumIRF3_z_MPTBA,CumIRF3_z_MPTBF, CumIRF4_z_MPTBA, CumIRF4_z_MPTBF,
     CI3_z_MPTBA,CI3_z_MPTBF, CI4_z_MPTBA, CI4_z_MPTBF,
     file = "savedRResults/Bootstrap_Oct3.RData")



#Initializing lists to store results
VARlags = list(ON_1mo = list(), ON_10y = list(), ON_30y = list(), 
               `1mo_10y` = list(), `1mo_30y` = list(), ON_1mo_30y = list())  
#VARs = VARs_z = CumIRFs = CumIRFs_z = Regs = ARIMAs = ARIMA_pVals = VARlags


VARSpec = list(c("TIIE","MPTBA","MXN_USD"),
               c("TIIE","GMXN10Y","MXN_USD"),
               c("TIIE","MPTBI","GMXN30Y","MXN_USD"),
               c("TIIE","MPTBC","GMXN30Y","MXN_USD"),
               c("TIIE","MPTBF","GMXN30Y","MXN_USD"),
               c("TIIE","MPTBA","GMXN30Y","MXN_USD"))

for(Spec in 6:6){
  # VARlags[[Spec]] = VARselect(Mex_d_TV_diff[VARSpec[[Spec]]], lag.max = 7, type = 'none')
  # print(c(VARSpec[[Spec]],
  #        paste(", lags:",VARlags[[Spec]]$selection['AIC(n)']) ))
  #  VARs[[Spec]] = VAR(Mex_w_TV[VARSpec[[Spec]]], p =5, type = "none")
  # print(summary(VARs[[Spec]]))
  # VARs[[Spec]] = tvVAR(Mex_d_TV_diff[VARSpec[[Spec]]], p =1, type = "none",
  #                      tkernel = "Epa", est = "lc", bw = c(20,20,20,20))
  # 
  VARs_z_ON_1mo = tvVAR(Mex_d_TV_diff[VARSpec[[Spec]]], p =1, type = "none",
                       z = Mex_d_TV$F_Own_p[-1],bw = rep(0.1,4))
  # # 
  # CumIRFs[[Spec]] = tvIRF(VARs[[Spec]], impulse = VARSpec[[Spec]][1:2],
  #                         response = VARSpec[[Spec]][2:3], cumulative = T)
  # CumIRFs_z[[Spec]] = tvIRF(VARs_z[[Spec]], impulse = VARSpec[[Spec]][1:2],
  #                           response = VARSpec[[Spec]][2:3], cumulative = T)
  CumIRFs_z_ON_1mo = tvIRF(VARs_z_ON_1mo, impulse = VARSpec[[Spec]][1:2],
                            response = VARSpec[[Spec]][2:3], cumulative = T)
  

  # if(Spec<4){
   # x = 1:length(CumIRFs[[Spec]]$irf$TIIE[,1,11])
   # Regs[[Spec]] = lm(CumIRFs_z[[Spec]]$irf$TIIE[,1,11]~ Mex_d_TV$F_Own_p[-c(1,2)]
   #                   + x)
   # ARON1moCum = Arima(MergedCumDat$IRF_ON1mo, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
   # 
  #  Regs[[Spec]] = lm(CumIRFs_z[[Spec]]$irf$MPTBA[,2,11]~ Mex_d_TV$F_Own_p[-c(1,2)]
  #                    )
    # ARIMAs[[Spec]] = Arima(CumIRFs[[Spec]]$irf$TIIE[,1,11], order = c(1,0,0),
    #                        xreg = Mex_d_TV$F_Own_p[-c(1,2)])
    # ARIMA_pVals[[Spec]] = (1-pnorm(abs(ARIMAs[[Spec]]$coef)/
    #                                  sqrt(diag(ARIMAs[[Spec]]$var.coef))))*2
  # } else{
     Regs[[Spec]] = lm(CumIRFs_z_ON_1mo$irf$TIIE[,1,11]~ Mex_d_TV$F_Own_p[-c(1,2)])
     ARIMAs[[Spec]] = Arima(CumIRFs_z_ON_1mo$irf$TIIE[,1,11], order = c(1,0,0),
                         xreg = Mex_d_TV$F_Own_p[-c(1,2)])
  #  }
  # ARIMA_pVals[[Spec]] = 
  #   (1-pnorm(abs(ARIMAs[[Spec]]$coef)/sqrt(diag(ARIMAs[[Spec]]$var.coef))))*2
  
  # print(summary(Regs[[Spec]]))
  # print(summary(ARIMAs[[Spec]]))
}

summary(Regs[[6]])
dwtest(Regs[[4]])

summary(ARIMAs[[6]])
dwtest(Regs[[4]])

IRF_CI = confint(CumIRFs_z[[5]])
IRF_CI_ON_1mo = confint(CumIRFs_z[[6]])
IRF_CI_ON_1mo_90 = confint(CumIRFs_z[[6]] ,level = 0.9)
beep(4)
save(VARs,CumIRFs, CumIRFs_z,IRF_CI,IRF_CI_ON_1mo, IRF_CI_ON_1mo_90, file = "TVVARResults_25092025.RData" )
load("TVVARResults_25092025.RData")
# Plotting Results --------------------------------------------------------


plot(Mex_d_TV$F_Own_p[-c(1,2)],CumIRFs_z[[Spec]]$irf$TIIE[,1,11])
plot( Mex_d_TV$F_Own_p[-c(1,2)],CumIRFs_z[[Spec]]$irf$MPTBF[,2,11])
plot(Mex_d_TV$F_Own_p)
plot(CumIRFs_z_ON_1mo$irf$TIIE[,1,11])
plot(CumIRFs[[Spec]]$irf$MPTBA[,2,11])
plot(CumIRFs[[Spec]]$irf$TIIE[,1,1])
IRFResults = data.frame(Date = Mex_d_TV_diff$Date[-1], 
           ON_1mo = CumIRFs[[6]]$irf$TIIE[,1,11],
           mo1_30Y = CumIRFs[[6]]$irf$MPTBA[,2,11],
           ON_3mo = CumIRFs[[4]]$irf$TIIE[,1,11],
           mo3_30Y = CumIRFs[[4]]$irf$MPTBC[,2,11],
           ON_6mo = CumIRFs[[5]]$irf$TIIE[,1,11],
           mo1_30Y = CumIRFs[[5]]$irf$MPTBF[,2,11],
           ON_9mo = CumIRFs[[3]]$irf$TIIE[,1,11],
           mo9_30Y = CumIRFs[[3]]$irf$MPTBI[,2,11],
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


# Plotting IRFs for Foreign Ownership -------------------------------------

plots = list(ON_9mo = list(), ON_3mo = list(), ON_6mo = list(), ON_1mo = list())
for (Spec in 6:6){
obs = dim(CumIRFs_z[[Spec]]$irf$TIIE)[1]
bottom10 = CumIRFs_z[[Spec]]$irf$TIIE[1:ceiling(0.1 * obs) , 1, ]
top10 = CumIRFs_z[[Spec]]$irf$TIIE[ceiling(0.9 * obs):obs , 1, ]
CumIRF_bottom10 = apply(bottom10,2,median)
CumIRF_top10 = apply(top10,2,median)

bottom10_L = apply(IRF_CI_ON_1mo_90$Lower$TIIE[1:ceiling(0.1 * obs),1,],2,median)
top10_L = apply(IRF_CI_ON_1mo_90$Lower$TIIE[ceiling(0.9 * obs):obs,1,],2,median)
bottom10_H = apply(IRF_CI_ON_1mo_90$Upper$TIIE[1:ceiling(0.1 * obs),1,],2,median)
top10_H = apply(IRF_CI_ON_1mo_90$Upper$TIIE[ceiling(0.9 * obs):obs,1,],2,median)
# CumirfDat <- data.frame(Period = 0:10,
#                          LowFo   = CumIRF_bottom10, HighFO = CumIRF_top10,
#                          LowFo_L   = bottom10_L, HighFO_L = top10_L,
#                          LowFo_H   = bottom10_H, HighFO_H = top10_H)
irf_long_TIIE <- data.frame(
  Period   = rep(0:10, 2),
  Spec     = rep(c("LowFO", "HighFO"), each = 11),
  Response = c(CumIRF_bottom10, CumIRF_top10),
  Lower    = c(bottom10_L, top10_L),
  Upper    = c(bottom10_H, top10_H)
)
# reshape to long format for ggplot
# irf_long_TIIE <- melt(CumirfDat, id.vars = "Period",
#                       variable.name = "Spec", value.name = "Response")

#plots[[Spec-2]]
x = ggplot(irf_long_TIIE, aes(x = Period, y = Response, color = Spec, fill = Spec)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, linewidth = 0) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  labs(x = "Horizon", y = "Cumulative IRF",
       #title = paste("Response of", VARSpec[[Spec]][2], "to ON shock")) +
       title = paste("Response of 1mo yield to ON shock")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
x
}
plots$ON_1mo


# Plotting without CIs ----------------------------------------------------

obs = dim(CumIRFs_z_ON_1mo$irf$MPTBA)[1]
bottom10 = CumIRFs_z_ON_1mo$irf$MPTBA[1:ceiling(0.3 * obs) , 2, ]
top10 = CumIRFs_z_ON_1mo$irf$MPTBA[ceiling(0.7 * obs):obs , 2, ]
CumIRF_bottom10 = apply(bottom10,2,median)*10
CumIRF_top10 = apply(top10,2,median)*10

CumirfDat <- data.frame(Period = 0:10, 
                        LowFo   = CumIRF_bottom10, HighFO = CumIRF_top10)
irf_long_TIIE <- melt(CumirfDat, id.vars = "Period",
                      variable.name = "Spec", value.name = "Response")
# plot
ggplot(irf_long_TIIE, aes(x = Period, y = Response, color = Spec)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  labs(x = "Horizon", y = "Cumulative IRF",
       title = "Response of 1mo to ON shock") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12), legend.position = "bottom")
# Regressions on cumIRFs --------------------------------------------------

regON1mo = lm(IRF1$irf$TIIE[,,11]~ Mex_w_TV$F_Own_p[-1])
summary(regON1mo)


VAR1 = tvVAR(Mex_w_TV[VARSpec[[1]]], p =1, type = "none" )
IRF1 = tvIRF(VAR1, impulse = "TIIE", response = "MPTBA", runs = 100, level = 0.68,
             cumulative = T)
IRF2 = tvIRF(VAR1, impulse = "TIIE", response = "MPTBA", runs = 100, level = 0.68)
