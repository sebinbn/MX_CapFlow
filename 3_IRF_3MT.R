# this code uses the list 'IRFs' created by 2_SVAR and creates a plot of contemporary
# responses the IRFs for various
# period and variable specifications


Maturity = rep("",17)
IRFVals = matrix(NaN,17,6)

i = 0
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

Maturity = paste("Yr",substr(Maturity,5,6))
Maturity[14:17] = c("Mo 01","Mo 03","Mo 09","Mo 06")

irfDat = data.frame(Maturity = Maturity, Values = IRFVals)
colnames(irfDat)[-1] = c("L_IRF","L_Up", "L_Low","H_IRF","H_Up", "H_Low") #intial L/H shows high/low FO period, IRF is value, Up and Low are upper and lower bounds respectively
                    

df_long <- melt(irfDat[c("Maturity","L_IRF","H_IRF")], 
                id.vars = 'Maturity', variable.name = 'Period')

# Create the ggplot with two lines
ggplot(df_long, aes(x = Maturity, y = value, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = 'Maturity', y = 'Transmission', title = 'Lower transmission') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.1, 0.9))

ggplot(df_long, aes(x = Maturity, y = value, fill = Period)) +
  geom_line(arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
  labs(x = 'Maturity', y = 'Transmission', title = 'Lower transmission') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(), legend.position.inside = c(0.1, 0.9))

