# This code uses the list 'IRFs' and 'samp_dates' created by 2_SVAR_MH and 
# creates a plot of contemporary responses from the IRFs for various periods. 
# Three plots are created; one for each combination of SVAR variables.

load("SVARResults_byHor.RData") #to load results if SVAR not run in current R session

Dates = rep(as.Date("2000-01-01"),length(samp_dates)) 
IRF_MH_Data = list() # a list to store the data for three pictures
CumIRF_MH_Data = list() # a list to store the data for three pictures

for (Spec in 1:3){
  
  IRFVals = matrix(NaN,length(samp_dates),3) #3 columns for IRF value, upper and lower bound
  CumIRFVals = matrix(NaN,length(samp_dates),1) #1 column for Cum IRF value
  for (i in 1:length(samp_dates)){
    if(Spec ==1){Dates[i] = samp_dates[[i]][1]}
    
    if (Spec == 3){
      IRFVals[i,1] = IRFs[[Spec]][[i]]$irf$MPTBA[,2][1] #picks out first obs of 2nd column from IRFs table
      IRFVals[i,2] = IRFs[[Spec]][[i]]$Upper$MPTBA[,2][1]
      IRFVals[i,3] = IRFs[[Spec]][[i]]$Lower$MPTBA[,2][1]
      
      CumIRFVals[i,1] = sum(IRFs[[Spec]][[i]]$irf$MPTBA[,2]) #sum of 2nd column from IRFs table
    }else{
      IRFVals[i,1] = IRFs[[Spec]][[i]]$irf$TIIE[,2][1] #picks out first obs of 2nd column from IRFs table
      IRFVals[i,2] = IRFs[[Spec]][[i]]$Upper$TIIE[,2][1]
      IRFVals[i,3] = IRFs[[Spec]][[i]]$Lower$TIIE[,2][1]
      
      CumIRFVals[i,1] = sum(IRFs[[Spec]][[i]]$irf$TIIE[,2]) #sum of 2nd column from IRFs table
    }
    
  }
  
  irfDat = data.frame(Dates = Dates, Values = IRFVals)
  colnames(irfDat)[-1] = c("IRF","Up", "Low") 
  IRF_MH_Data = c(IRF_MH_Data, list(irfDat))
  
  CumIRFDat = data.frame(Dates = Dates, Values = CumIRFVals)
  colnames(CumIRFDat)[-1] = "CumIRF" 
  CumIRF_MH_Data = c(CumIRF_MH_Data, list(CumIRFDat))
 
} #end of Spec loop 


# Creating Plots ----------------------------------------------------------

## Unified plots ---------------------------------------------------------

FO_avg = data.frame(IRF_MH_Data[[1]]["Dates"], FO = matrix(NaN,length(samp_dates),1) )
for(i in 1:length(samp_dates)){
 FO_avg$FO[i] = mean(Mex_FO$F_Own_p[Mex_FO$Date>= samp_dates[[i]][1] & 
                                      Mex_FO$Date <= samp_dates[[i]][2]])
}

MergedDat = merge(IRF_MH_Data[[1]][c("Dates", "IRF")],
                  IRF_MH_Data[[3]][c("Dates", "IRF")], by = "Dates")
MergedDat = merge(MergedDat,FO_avg, by = "Dates")
MergedDat$IRF.x = c(MergedDat$IRF.x[1], rollmean(MergedDat$IRF.x,2) )
MergedDat$IRF.y = c(MergedDat$IRF.x[1], rollmean(MergedDat$IRF.y,2) )

MergedDat = MergedDat[MergedDat$Dates<= as.Date("2015-12-31") & 
                        MergedDat$Dates >= as.Date("2010-01-01"),]

Merged_long1 = melt(MergedDat[-3], id.vars = "Dates", variable.name = "Spec")
MergedDat$FO = MergedDat$FO * 1.5 # a scaling to display both lines in 1 graph neatly for 2nd graph
Merged_long2 = melt(MergedDat[-2], id.vars = "Dates", variable.name = "Spec")

Plot_ON1mo = ggplot(Merged_long1 , aes(x = Dates, y = value, color = Spec)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(name = 'Contemporaneous Transmission (in pp.)',
                     sec.axis = sec_axis(transform=~., name="Prop. bonds foreign owned"))+
  scale_color_discrete(labels = c("IRF.x" = "Impulse Response", 
                                  "FO" = "Prop. bonds Foreign owned")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(x = element_blank(), title = 'Imp = ON, Resp = 1mo') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_blank())

Plot_1mo10y = ggplot(Merged_long2 , aes(x = Dates, y = value, color = Spec)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(name = 'Contemporaneous Transmission (in pp.)',
                     sec.axis = sec_axis(transform=~./1.5, name="Propn. bonds foreign owned"))+
  scale_color_discrete(labels = c("IRF.y" = "Impulse Response", 
                                  "FO" = "Propn. bonds foreign owned ")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(x = element_blank(),title = 'Imp = 1mo, Resp = 10yr') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45,hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_blank())

ggarrange(Plot_1mo10y, Plot_ON1mo, common.legend = T, legend = "bottom")

## Plotting ON_1mo ---------------------------------------------------------

  ggplot(IRF_MH_Data[[1]], aes(x = Dates, y = IRF)) +
    geom_line(linewidth = 1.2) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
    labs(x = element_blank(), y = 'Contemporaneous Transmission (in % points)',
         title = 'Impulse = Overnight rate, Response = 1 mo yield') +
    theme_minimal()+
    theme(title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_blank(), legend.position = c(0.9, 0.9))

## Plotting ON_10y ---------------------------------------------------------
ggplot(IRF_MH_Data[[2]], aes(x = Dates, y = IRF)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  labs(x = element_blank(), y = 'Contemporaneous Transmission (in % points)',
       title = 'Impulse = Overnight rate, Response = 10 yr yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))

## Plotting 1mo_10y ---------------------------------------------------------
ggplot(IRF_MH_Data[[3]], aes(x = Dates, y = IRF)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  labs(x = element_blank(), y = 'Contemporaneous Transmission (in % points)',
       title = 'Impulse = 1 mo yield, Response = 10 yr yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))

# Creating Cumulative Plots ----------------------------------------------------------

## Plotting ON_1mo ---------------------------------------------------------

ggplot(CumIRF_MH_Data[[1]], aes(x = Dates, y = CumIRF)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  labs(x = element_blank(),y = 'Cumulative Transmission (in % points)',
       title = 'Impulse = Overnight rate, Response = 1 mo yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))

## Plotting ON_10y ---------------------------------------------------------
ggplot(CumIRF_MH_Data[[2]], aes(x = Dates, y = CumIRF)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  labs(x = element_blank(), y = 'Cumulative Transmission (in % points)',
       title = 'Impulse = Overnight rate, Response = 10 yr yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))

## Plotting 1mo_10y ---------------------------------------------------------
ggplot(CumIRF_MH_Data[[3]], aes(x = Dates, y = CumIRF)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  labs(x = element_blank(), y = 'Cumulative Transmission (in % points)',
       title = 'Impulse = 1 mo yield, Response = 10 yr yield') +
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.9, 0.9))



