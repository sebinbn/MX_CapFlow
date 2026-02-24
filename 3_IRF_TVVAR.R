# This file plots IRFs with confidence intervals for specified quintiles using 
# IRF data generated in 2_TVVAR_v1.R

#load("savedRResults/Bootstrap_Oct3.RData")
load("savedRResults/tvVARResults_Oct17.RData")


# Function to plot IRFs at quinitles of FO --------------------------------


IRFGenerator = function(FO_data, IRFobj, qntile ){

  #Find value of FO at each quintile
  FO_qnt = quantile(FO_data, probs = qntile)
  
  #Picking mean IRFs and mean upper and lower limits
  LowFO = apply(IRFobj$irf$TIIE[which(FO_data > FO_qnt[1] &
                                     FO_data < FO_qnt[2]),1, ],2,mean)
  HighFO = apply(IRFobj$irf$TIIE[which(FO_data > FO_qnt[3] &
                                      FO_data < FO_qnt[4]),1, ],2,mean)
  LowFO_L = apply(IRFobj$Lower$TIIE[which(FO_data > FO_qnt[1] &
                                         FO_data < FO_qnt[2]),1, ],2,mean)
  HighFO_L = apply(IRFobj$Lower$TIIE[which(FO_data > FO_qnt[3] &
                                          FO_data < FO_qnt[4]),1, ],2,mean)
  LowFO_H = apply(IRFobj$Upper$TIIE[which(FO_data > FO_qnt[1] &
                                         FO_data < FO_qnt[2] ),1, ],2,mean)
  HighFO_H = apply(IRFobj$Upper$TIIE[which(FO_data > FO_qnt[3] &
                                          FO_data < FO_qnt[4]),1, ],2,mean)
  
  irf_long_TIIE <- data.frame(
    Period   = rep(0:10, 2),
    Spec     = rep(c("Low FO period", "High FO period"), each = 11),
    Response = c(LowFO, HighFO),
    Lower    = c(LowFO_L, HighFO_L),
    Upper    = c(LowFO_H, HighFO_H)
  )
  
  IRFplot = ggplot(irf_long_TIIE, aes(x = Period, y = Response, color = Spec, fill = Spec)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, linewidth = 0) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    labs(x = "Horizon", y = "Cumulative IRF")+
    #title = paste("Response of",resp, "yield to ON shock")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.position = "bottom")
  return(IRFplot)
}

# Function is provided 3 inputs:
# 1. Subset of F_Own_p for which IRFs exist after running VAR(5) on first differences.
# 2. The list where tvIRFs are stored.
# 3. The two pairs of quintiles whose means are taken
CIRF_pic = IRFGenerator(Mex_d_TV[-c(1:6),"F_Own_p"], CI_1mo,
                        qntile = c(0.05,0.15,0.85,0.95))
CIRF_pic

# Creating function for generating IRF ------------------------------------

IRFGenerator = function(obj, resp, pcnt, gap){
  obs = dim(obj$irf$TIIE)[1]
  LowFO = apply(obj$irf$TIIE[(ceiling((pcnt[1]-gap) * obs)+1):ceiling((pcnt[1]+gap) * obs),
                             1, ],2,median)
  HighFO = apply(obj$irf$TIIE[(ceiling((pcnt[2]-gap) * obs)+1):ceiling((pcnt[2]+gap) * obs),
                              1, ],2,median)
  
  LowFO_L = apply(obj$Lower$TIIE[(ceiling((pcnt[1]-gap) * obs)+1):ceiling((pcnt[1]+gap) * obs)
                                 ,1,],2,median)
  HighFO_L = apply(obj$Lower$TIIE[(ceiling((pcnt[2]-gap) * obs)+1):ceiling((pcnt[2]+gap) * obs)
                                  ,1,],2,median)
  LowFO_H = apply(obj$Upper$TIIE[(ceiling((pcnt[1]-gap) * obs)+1):ceiling((pcnt[1]+gap) * obs)
                                 ,1,],2,median)
  HighFO_H = apply(obj$Upper$TIIE[(ceiling((pcnt[2]-gap) * obs)+1):ceiling((pcnt[2]+gap) * obs)
                                  ,1,],2,median)
  
  irf_long_TIIE <- data.frame(
    Period   = rep(0:10, 2),
    Spec     = rep(c("LowFO", "HighFO"), each = 11),
    Response = c(LowFO, HighFO),
    Lower    = c(LowFO_L, HighFO_L),
    Upper    = c(LowFO_H, HighFO_H)
  )
 
  x = ggplot(irf_long_TIIE, aes(x = Period, y = Response, color = Spec, fill = Spec)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, linewidth = 0) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    labs(x = "Horizon", y = "Cumulative IRF")+
         #title = paste("Response of",resp, "yield to ON shock")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.position = "bottom")
  return(x)
}


# p1- VAR(5) and p5 - VAR(1). B1 - wild2 - normal and B2 - wild - empirical
obj= CI4_z_MPTBA$p1B2
pic = IRFGenerator(CI4_z_MPTBA$p1B2, resp = "1mo", pcnt = c(0.1,0.9), gap = 0.05)
pic


# Creating 3-D CIRF ------------------------------------------------------

plot(obj$x$z, obj$irf$TIIE[ , 2,11]) #checking 2-D images

line(obj$x$z)
library(plotly)


plot_ly(data.frame(obj$irf$TIIE[,2,]),
   x = ~0:10,               
   y = obj$x$z,              
   z = obj$irf$TIIE[ ,2,],               
  type = "surface",
  colorscale = "Viridis",
  colorbar = list(title = list( text = "Cumulative IRF",font = list(size = 16)),
                  tickfont = list(size = 14),len = 0.7, 
                  x = 0.95, xanchor = "left")
) %>%
  layout(
    scene = list(
      xaxis = list(title = list(text = "Horizon (0–10)", font = list(size = 18)),
                   tickfont = list(size = 14)),
      yaxis = list(title = list(text ="Propn. of FO Bonds",  font = list(size = 18)),
                   tickfont = list(size = 14)),
      zaxis = list(title = list(text ="Cumulative IRF", font = list(size = 18)),
                   tickfont = list(size = 14))     
      )  )

# Plotting IRF against FO -------------------------------------------------

obj = CumIRF_1mo_bw
CIRFvsFOTab = data.frame( Date = Mex_d_TV_diff$Date[-c(1:5)],
                  CIRF   = obj$irf$TIIE[,2, 11],  FO    = obj$x$z )
CIRFvsFOTab$FO_scaled = CIRFvsFOTab$FO/10  # Rescaling Fo to match IRF scale for plotting


ggplot(CIRFvsFOTab, aes(x = Date)) +
  geom_line(aes(y = CIRF, color = "CIRF"), linewidth = 1.2) +
  geom_line(aes(y = FO_scaled, color = "FO_scaled"), linewidth = 1.2) +
  scale_y_continuous(name = "Cumulative IRF after 10 days",
    sec.axis = sec_axis(~.*10,
      name = "Proportion of bonds Foreign Owned (FO)")  ) +
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


# Redacted code -----------------------------------------------------------


# IRFGenerator1 = function(obj, resp){ #to plot impact of short term on long term
#   obs = dim(obj$irf$MPTBF)[1]
#   bottom10 = obj$irf$MPTBF[ceiling(0.05 * obs):ceiling(0.15 * obs) , 2, ]
#   top10 = obj$irf$MPTBF[ceiling(0.85 * obs):ceiling(0.95 * obs) , 2, ]
#   CumIRF_bottom10 = apply(bottom10,2,median)
#   CumIRF_top10 = apply(top10,2,median)
#   
#   bottom10_L = apply(obj$Lower$MPTBF[ceiling(0.05 * obs):ceiling(0.15 * obs),2,],2,median)
#   top10_L = apply(obj$Lower$MPTBF[ceiling(0.85 * obs):ceiling(0.95 * obs),2,],2,median)
#   bottom10_H = apply(obj$Upper$MPTBF[ceiling(0.05 * obs):ceiling(0.15 * obs),2,],2,median)
#   top10_H = apply(obj$Upper$MPTBF[ceiling(0.85 * obs):ceiling(0.95 * obs),2,],2,median)
#   
#   irf_long_MPTBF <- data.frame(
#     Period   = rep(0:10, 2),
#     Spec     = rep(c("LowFO", "HighFO"), each = 11),
#     Response = c(CumIRF_bottom10, CumIRF_top10),
#     Lower    = c(bottom10_L, top10_L),
#     Upper    = c(bottom10_H, top10_H)
#   )
#   
#   x = ggplot(irf_long_MPTBF, aes(x = Period, y = Response, color = Spec, fill = Spec)) +
#     geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, linewidth = 0) +
#     geom_line(linewidth = 1.2) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#     scale_x_continuous(breaks = c(0, 5, 10)) +
#     labs(x = "Horizon", y = "Cumulative IRF",
#          title = paste("Response of",resp, "yield to 6mo shock")) +
#     theme_minimal() +
#     theme(axis.text = element_text(size = 14),
#           axis.title = element_text(size = 14),
#           legend.title = element_blank(),
#           legend.text = element_text(size = 12),
#           legend.position = "bottom")
#   return(x)
# }