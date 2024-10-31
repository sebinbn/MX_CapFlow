# this code uses the list 'IRFs' and 'samp_dates' created by 2_SVAR_MH and creates a plot of contemporary
# responses from the IRFs for various periods


Dates = rep(as.Date("2000-01-01"),length(samp_dates)) 
IRFVals = matrix(NaN,length(samp_dates),3) #3 columns for IRF value, upper and lower bound

for (i in 1:length(samp_dates)){
  Dates[i] = samp_dates[[i]][1]
  # IRFVals[i,1] = IRFs[[i]]$irf$MPTBA[,2][1] #picks out first obs of 2nd column from IRFs table
  # IRFVals[i,2] = IRFs[[i]]$Upper$MPTBA[,2][1]
  # IRFVals[i,3] = IRFs[[i]]$Lower$MPTBA[,2][1]
  
  IRFVals[i,1] = IRFs[[i]]$irf$TIIE[,2][1] #picks out first obs of 2nd column from IRFs table
  IRFVals[i,2] = IRFs[[i]]$Upper$TIIE[,2][1]
  IRFVals[i,3] = IRFs[[i]]$Lower$TIIE[,2][1]
}

irfDat = data.frame(Dates = Dates, Values = IRFVals)
colnames(irfDat)[-1] = c("IRF","Up", "Low") 


ggplot(irfDat, aes(x = Dates, y = IRF)) +
  geom_line(linewidth = 1.2) +
  labs(x = 'Date', y = 'Contemporaneous Transmission (in % points)',
       title = 'Impulse = Overnight rate, Response = 1 mo yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))



