# This code uses the list 'IRFs' and 'samp_dates' created by 2_SVAR_MH to create
# MergedDat and MergedCumDat

#load("SVARResultsMovHorzn.RData") #to load results if SVAR not run in current R session


# Combining SVAR results in a list ----------------------------------------

Dates = rep(as.Date("2000-01-01"),length(samp_dates)) 
IRF_MH_Data = list()    # a list to store the data for three pictures
CumIRF_MH_Data = list()    # a list to store the data for three pictures

for (Spec in 1:5){
  
  IRFVals = matrix(NaN,length(samp_dates),3) #3 columns for IRF value, upper and lower bound
  CumIRFVals = matrix(NaN,length(samp_dates),1) #1 column for Cum IRF value
  for (i in 1:length(samp_dates)){
    if(Spec ==1){Dates[i] = samp_dates[[i]][1]} #dates need be added only for 1 spec as it is same for other 2
    
    if (Spec > 3){ #SVARs where 1mo is short rate
      IRFVals[i,1] = IRFs[[Spec]][[i]]$irf$MPTBA[,2][1] #picks out first obs of 2nd column from IRFs table
      IRFVals[i,2] = IRFs[[Spec]][[i]]$Upper$MPTBA[,2][1]
      IRFVals[i,3] = IRFs[[Spec]][[i]]$Lower$MPTBA[,2][1]
      
      CumIRFVals[i,1] = sum(IRFs[[Spec]][[i]]$irf$MPTBA[,2]) #sum of 2nd column from IRFs table
    }else{ #SVARs where ON is short rate
      IRFVals[i,1] = IRFs[[Spec]][[i]]$irf$TIIE[,2][1] #picks out first obs of 2nd column from IRFs table
      IRFVals[i,2] = IRFs[[Spec]][[i]]$Upper$TIIE[,2][1]
      IRFVals[i,3] = IRFs[[Spec]][[i]]$Lower$TIIE[,2][1]
      
      CumIRFVals[i,1] = sum(IRFs[[Spec]][[i]]$irf$TIIE[,2]) #sum of 2nd column from IRFs table
    }
    
  }#end of date loop
  
  irfDat = data.frame(Dates = Dates, Values = IRFVals)
  colnames(irfDat)[-1] = c("IRF","Up", "Low") 
  IRF_MH_Data = c(IRF_MH_Data, list(irfDat))
  
  CumIRFDat = data.frame(Dates = Dates, Values = CumIRFVals)
  colnames(CumIRFDat)[-1] = "CumIRF" 
  CumIRF_MH_Data = c(CumIRF_MH_Data, list(CumIRFDat))
  
} #end of Spec loop 


# Making tables by merging with FO data -----------------------------------


FO_avg = data.frame(IRF_MH_Data[[1]]["Dates"]) #copying dates column

# Creating FO as proportion and as value
for(i in 1:length(samp_dates)){
  FO_avg$FO_prop[i] = mean(Mex_FO$F_Own_p[Mex_FO$Date>= samp_dates[[i]][1] &
                                       Mex_FO$Date <= samp_dates[[i]][2]])* 10
  # multiply by 10 to convert share to 10% scale so that regression coefficients become responses
  # to 10% increase in ownership percent
  FO_avg$FO_val[i] = mean(Mex_FO$F_Own[Mex_FO$Date>= samp_dates[[i]][1] & 
                                         Mex_FO$Date <= samp_dates[[i]][2]])/1000000  
  
}


MergedDat = merge(IRF_MH_Data[[1]][c("Dates", "IRF")],
                  IRF_MH_Data[[2]][c("Dates", "IRF")], by ="Dates")
for (i in 3:5){
  MergedDat = merge(MergedDat,IRF_MH_Data[[i]][c("Dates", "IRF")], by = "Dates")
}
MergedDat = merge(MergedDat,FO_avg, by = "Dates")
colnames(MergedDat) = c("StartDate","IRF_ON1mo","IRF_ON10y","IRF_ON30y","IRF_1mo10y",
                        "IRF_1mo30y","FO_prop","FO_val(Mn)")

#Creating cumulative data table
MergedCumDat = merge(CumIRF_MH_Data[[1]],CumIRF_MH_Data[[2]], by = "Dates")
for (i in 3:5){
  MergedCumDat = merge(MergedCumDat,CumIRF_MH_Data[[i]], by = "Dates")
}
MergedCumDat = merge(MergedCumDat,FO_avg, by = "Dates")
colnames(MergedCumDat) = c("StartDate","IRF_ON1mo","IRF_ON10y","IRF_ON30y","IRF_1mo10y",
                           "IRF_1mo30y","FO_prop","FO_val(Mn)")


