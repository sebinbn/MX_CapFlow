# this code uses the list 'IRFs' created by 2_SVAR and creates a plot of contemporary
# responses from the IRFs for various period and variable specifications

# If running this file separately using IRFs estimated in an earlier session, load 
# results from below
load("SVARResults.RData")

Maturity = rep("",18) # 17 maturities where SVAR is done and 1 for the impulse (ON rate)
IRFVals = matrix(NaN,18,6) #6 columns for IRF value, upper and lower bound for high and low FO
CumIRFVals = matrix(NaN, 18,2) #6 columns for cumulative IRF value from high and low FO

Maturity[1] = "1 Day"
IRFVals[1,] = rep(1,6)
CumIRFVals[1,] = c(sum(IRFs[[1]]$irf$TIIE[,1]), sum(IRFs[[2]]$irf$TIIE[,1]) )
i = 1
for (samp_num in seq(1,49,3)){
  
  i = i + 1
  Maturity[i] = colnames(IRFs[[samp_num]]$irf$TIIE)[2] 
  IRFVals[i,1] = IRFs[[samp_num]]$irf$TIIE[,2][1] #picks out first obs of 2nd column from IRFs table
  IRFVals[i,2] = IRFs[[samp_num]]$Upper$TIIE[,2][1]
  IRFVals[i,3] = IRFs[[samp_num]]$Lower$TIIE[,2][1]
  
  CumIRFVals[i,1] = sum(IRFs[[samp_num]]$irf$TIIE[,2])
  
    #Picking out 2012-13 values
  IRFVals[i,4] = IRFs[[samp_num+1]]$irf$TIIE[,2][1] 
  IRFVals[i,5] = IRFs[[samp_num+1]]$Upper$TIIE[,2][1]
  IRFVals[i,6] = IRFs[[samp_num+1]]$Lower$TIIE[,2][1]
  
  CumIRFVals[i,2] = sum(IRFs[[samp_num+1]]$irf$TIIE[,2])
}

Maturity[-1] = paste(substr(Maturity[-1],5,6), "Yr")
Maturity[15:18] = c("1 Mo","3 Mo","9 Mo","6 Mo")

irfDat = data.frame(Maturity = Maturity, Values = IRFVals)
cumIRFDat = data.frame(Maturity = Maturity, Values = CumIRFVals)
colnames(irfDat)[-1] = c("L_IRF","L_Up", "L_Low","H_IRF","H_Up", "H_Low") #intial L/H shows high/low FO period, IRF is value, Up and Low are upper and lower bounds respectively
colnames(cumIRFDat)[-1] = c("L_CIRF","H_CIRF")
irfDat$Maturity = factor(irfDat$Maturity, 
                         levels = Maturity[c(1,15,16,18,17,2:14)])         #converting to factor to control order on x-axis.           
cumIRFDat$Maturity = irfDat$Maturity



# Plotting 1 week impact --------------------------------------------------

irf_long <- melt(irfDat[-1,c("Maturity","L_IRF","H_IRF")], 
                id.vars = 'Maturity', variable.name = 'Period')

ggplot(irf_long, aes(x = Maturity, y = value, fill = Period)) +
  geom_segment(aes(x = Maturity, xend = Maturity, y = 0, yend = value, color = Period),
               linewidth = 1.75,
               arrow = arrow(length = unit(0.15, "inches"))) +
  scale_color_discrete(labels = c("L_IRF" = "Low Foreign Ownership", 
                                  "H_IRF" = "High Foreign Ownership")) +
  labs(x = 'Maturity', y = 'Response (in % points)',
       title = 'Response of yields to 1 % point increase in overnight rate') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank(), legend.position = c(0.77, 0.9))


# Plotting cumulative impact ----------------------------------------------

cumIRF_long <- melt(cumIRFDat[-1,c("Maturity","L_CIRF","H_CIRF")], 
                    id.vars = 'Maturity', variable.name = 'Period')
ggplot(cumIRF_long, aes(x = Maturity, y = value, fill = Period)) +
  geom_segment(aes(x = Maturity, xend = Maturity, y = 0, yend = value, color = Period),
               linewidth = 1.75,
               arrow = arrow(length = unit(0.15, "inches"))) +
  scale_color_discrete(labels = c("L_CIRF" = "Low Foreign Ownership", 
                                  "H_CIRF" = "High Foreign Ownership")) +
  labs(x = 'Reamining Maturity', y = 'Response (in % points)',
       title = 'Cumulative response of yields to 1pp increase in Overnight rate') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.title = element_blank(), legend.position = c(0.77, 0.9))


