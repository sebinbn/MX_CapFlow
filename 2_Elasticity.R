# This code calculates price elasticity of various owners to TIIE, MPTBA and 
# GMXN10Y. It uses BBYield, Own_Data and TIIE to calculate price elasticity
# from daily data.It uses Own_Data and Mex_W to calculate price elasticity from
# weekly data. The table in paper reports the daily results.


DateIndex_A = BBYield$Date <= as.Date("2023-12-31") & 
                      BBYield$Date >= as.Date("2006-01-01")
DateIndex_B = Own_Data$Date <= as.Date("2023-12-31") & 
                      Own_Data$Date >= as.Date("2006-01-01")
DateIndex_C = TIIE$Date <= as.Date("2023-12-31") & 
                      TIIE$Date >= as.Date("2006-01-01")
VarsOwn = c("Date","SF65218","SF65213","SF65215", "SF65214", "SF65211", "Banxico",
            "SF65216", "SF65217")

# Daily data Analysis ------------------------------------------------------

# Creating daily data -----------------------------------------------------

RegData = merge(BBYield[DateIndex_A, c("Date","GMXN10Y","MPTBA")], 
                Own_Data[DateIndex_B, VarsOwn] )
RegData = merge(RegData,TIIE[DateIndex_C,c("Date","TIIE")])
colMeans(is.na(RegData))                                                          #checking if NAs exist
summary(RegData)
RegData[RegData$SF65215 == 0,"SF65215"] = 10^-5 

#RegData[c("TIIE","MPTBA","GMXN10Y")] = RegData[c("TIIE","MPTBA","GMXN10Y")]/100   #converting yields in % to decimal values so as to interpret coefficients in regression

## Price elasticity calculation --------------------------------------------

Elast_d = matrix(NA,9,8 )
xvar = c("TIIE","MPTBA","GMXN10Y" )
for (x in 1:3 ){
  for (y in 1:8){
    z = na.omit(RegData[c(VarsOwn[1+y],xvar[x])])
    reg = lm(log(z[,1]) ~ z[,2])
    Elast_d[c(3*x-2,3*x-1,3*x),y] = coef(summary(reg))[2,c("Estimate","Std. Error", "Pr(>|t|)")]
  }
}
Tab_Elast_d = data.frame(Particular = rep(c("coefficient", "Std. Error", "p-value"), 3),
                        Owner = Elast_d)
Tab_Elast_d$Particular[c(1,4,7)] =  c("TIIE","Y_1mo","Y_10Yr" )
colnames(Tab_Elast_d)[-1] = c("Non-residents", "Pension funds", "Insurance Co.", 
                      "Invst. funds", "Banks", "Banxico","Others", "Residents") 

# Weekly data Analysis -----------------------------------------------------

# Creating weekly data ----------------------------------------------------

Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

DateIndex_A = BBYield$Date <= as.Date("2023-12-31") & 
  BBYield$Date >= as.Date("2005-12-24")                                           #extracting data from last week of 2005 to calculate weekly value for 2006-01-01
DateIndex_B = Own_Data$Date <= as.Date("2023-12-31") & 
  Own_Data$Date >= as.Date("2005-12-24")
DateIndex_C = TIIE$Date <= as.Date("2023-12-31") & 
  TIIE$Date >= as.Date("2005-12-24")

Own_Data_w = data.frame(Date = Sun, Values = matrix(NA, length(Sun),
                                                    length(VarsOwn)-1))                #Initiating a dataframe
colnames(Own_Data_w)[-1] = VarsOwn[-1]

for(i in 1:length(Sun)){                                                          
  Week_Data = Own_Data[Own_Data$Date <= Sun[i] & Own_Data$Date > (Sun[i]-7),
                        VarsOwn]
  
  for (V in VarsOwn[-1]){
    a = na.omit(Week_Data[V])
    Own_Data_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
}

RegData_w = merge(Mex_w[c("Date", "TIIE", "MPTBA", "GMXN10Y")], Own_Data_w)
colMeans(is.na(RegData_w))
summary(RegData_w)
RegData_w[RegData_w$SF65215 == 0,"SF65215"] = 10^-5 

## Price elasticity calculation --------------------------------------------

Elast_w = matrix(NA,9,8 )
for (x in 1:3 ){
  for (y in 1:8){
    z = na.omit(RegData_w[c(VarsOwn[1+y],xvar[x])])
    reg = lm(log(z[,1]) ~ z[,2])
    Elast_w[c(3*x-2,3*x-1,3*x),y] = coef(summary(reg))[2,c("Estimate","Std. Error", "Pr(>|t|)")]
  }
}
Tab_Elast_w = data.frame(Particular = Tab_Elast_d$Particular,Owner = Elast_w)
colnames(Tab_Elast_w)[-1] = c("Non-residents", "Pension funds", "Insurance Co.", 
                              "Invst. funds", "Banks", "Banxico","Others", "Residents") 


# Removing excess variables -----------------------------------------------

rm(DateIndex_A, DateIndex_B, DateIndex_C, reg, Sun, Elast_d, Elast_w, Week_Data,
   z, RegData, RegData_w, x, y, xvar, VarsOwn)
