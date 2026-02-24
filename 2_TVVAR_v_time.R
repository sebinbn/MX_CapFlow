# This file runs TvVAR with time as the weighting variable. This is done to generate 
# a motivation picture.However, after trying various bandwidths, I find that I 
# am unable to generate a picture useful for the motivation page.


VAR_1mo = CumIRF_1mo =  list()
VAR_vars = c("TIIE","MPTBA","GMXN30Y", "MXN_USD")

start_time = Sys.time()
# bw calculated on its own gives  2.194735 0.7626812 20 20 
VAR_1mo_t = tvVAR(Mex_d_TV_diff[VAR_vars],p = 5, type = "none", tkernel = "Epa")
# bw for all set at 20
VAR_1mo_t_bw = tvVAR(Mex_d_TV_diff[VAR_vars],p = 5, type = "none", tkernel = "Epa",
                     bw = rep(20,4))
print(paste("VAR(5) estimated in", 
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))
start_time = Sys.time()
CumIRF_1mo_t  = SN_tvIRF(VAR_1mo_t, cumulative = T,  unit.shock = T,
                          impulse =VAR_vars, response = VAR_vars)
CumIRF_1mo_t_bw  = SN_tvIRF(VAR_1mo_t_bw, cumulative = T, bw.cov = 20,  unit.shock = T,
                         impulse =VAR_vars, response = VAR_vars)

print(paste("IRF(5) estimated in", 
            round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))

save(VAR_1mo_t,VAR_1mo_t_bw,CumIRF_1mo_t,CumIRF_1mo_t_bw, 
     file = "SavedRResults/2_TVVAR_v_time_Results_290126.RData")

# start_time = Sys.time()
# CI_1mo_1 = SN_CI_tvirf(CumIRF_1mo, level = 0.68, tboot = "wild")
# print(paste("Confint for IRF(5) with wild errors estimated in",
#             round(difftime(Sys.time(), start_time, units = "mins"),2), "mins"))

obj = CumIRF_1mo_t_bw
CIRFTab = data.frame( Date = Mex_d_TV_diff$Date[-c(1:5)],
                          CIRF   = obj$irf$TIIE[,2, 11])

ggplot(CIRFTab, aes(x = Date)) +
  geom_line(aes(y = CIRF, color = "CIRF"), linewidth = 1.2) +
  #geom_line(aes(y = FO_scaled, color = "FO_scaled"), linewidth = 1.2) +
  scale_y_continuous(name = "Cumulative IRF after 10 days") +
  scale_color_manual(values = c("CIRF" = "blue","FO_scaled" = "red"),
                     labels = c("CIRF" = "CIRF(10)", "FO_scaled" = "Bonds FO")) +
  theme_minimal() +
  labs(x = "Time",title = "Cumulative impact of policy rate shock after 10 days",
       color = "") +
  theme(
    axis.title.y.left  = element_text(color = "blue", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )
