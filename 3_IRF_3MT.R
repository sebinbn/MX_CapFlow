# this code uses the list 'IRFs' created by 2_SVAR and creates a plot of contemporary
# responses from the IRFs for various period and variable specifications


Maturity = rep("",18) # 17 maturities where SVAR is done and 1 for the impulse (ON rate)
IRFVals = matrix(NaN,18,6) #6 columns for IRF value, upper and lower bound for high and low FO
Maturity[1] = "1 Day"
IRFVals[1,] = rep(1,6)
i = 1
for (samp_num in seq(1,49,3)){
  i = i + 1
  Maturity[i] = colnames(IRFs[[samp_num]]$irf$TIIE)[2] 
  IRFVals[i,1] = IRFs[[samp_num]]$irf$TIIE[,2][1] #picks out first obs of 2nd column from IRFs table
  IRFVals[i,2] = IRFs[[samp_num]]$Upper$TIIE[,2][1]
  IRFVals[i,3] = IRFs[[samp_num]]$Lower$TIIE[,2][1]
  #Picking out 2012-13 values
  IRFVals[i,4] = IRFs[[samp_num+1]]$irf$TIIE[,2][1] 
  IRFVals[i,5] = IRFs[[samp_num+1]]$Upper$TIIE[,2][1]
  IRFVals[i,6] = IRFs[[samp_num+1]]$Lower$TIIE[,2][1]
}

Maturity[-1] = paste(substr(Maturity[-1],5,6), "Yr")
Maturity[15:18] = c("1 Mo","3 Mo","9 Mo","6 Mo")

irfDat = data.frame(Maturity = Maturity, Values = IRFVals)
colnames(irfDat)[-1] = c("L_IRF","L_Up", "L_Low","H_IRF","H_Up", "H_Low") #intial L/H shows high/low FO period, IRF is value, Up and Low are upper and lower bounds respectively
irfDat$Maturity = factor(irfDat$Maturity, 
                         levels = Maturity[c(1,15,16,18,17,2:14)])         #converting to factor to control order on x-axis.           

df_long <- melt(irfDat[c("Maturity","L_IRF","H_IRF")], 
                id.vars = 'Maturity', variable.name = 'Period')


ggplot(df_long, aes(x = Maturity, y = value, fill = Period)) +
  geom_segment(aes(x = Maturity, xend = Maturity, y = 0, yend = value, color = Period),
               size = 1.5,
               arrow = arrow(length = unit(0.15, "inches"))) +
  scale_color_discrete(labels = c("L_IRF" = "Low FO", "H_IRF" = "High FO")) +
  labs(x = 'Maturity', y = 'Transmission (in % points)',
       title = 'Lower transmission in High foreign ownership period') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))



