# This file merges the various data to create a single table to be used in SVAR.
# It creaetes the daily frequency table and then the weekly frequency table using
# end of week daily data.


# Creating Merged table ---------------------------------------------------

MergedTab  = merge(BBYield, Mex_FO)
MergedTab = merge(MergedTab, MXN)
MergedTab = merge(MergedTab, TIIE, all= T)

VarsExport = c( "Date","MPTBA", "GMXN10Y", "MXN_USD","TIIE", "F_Own_p","Tgt_rate")

# Creating Daily Data  ----------------------------------------------------

Mex_d = MergedTab[MergedTab$Date <= as.Date("2023-12-31") & 
                    MergedTab$Date >= as.Date("2006-01-01"), VarsExport]            #Creating subset of  daily data for SVAR required variables from 2006
                 
write.csv(Mex_d, "Mexicopaper_Data/Mex_Daily.csv", row.names = F)                                 #Exporting daily data

# Creating Weekly Data  ---------------------------------------------------

# LOGIC
# Creating weekly data as the data on the latest trading day of the week. First,
# all sunday dates are created and this is used to extract the weekly table. From
# the weekly table, the last value is picked of each variable and stored in Mex_w.
# Note: TIIE starts only from 2006-01-02 and so has an NA for 2006-01-01 which is 
# a Sunday


Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

Mex_w = data.frame(Date = Sun, Values = matrix(NA, length(Sun),6))                #Initiating a dataframe
colnames(Mex_w) = VarsExport

colMeans(is.na(Mex_d))                                                            #checking which variables have missing values - only MTBA and GMXN10y have missing values
length(Mex_d$Date[is.na(Mex_d$Tgt_rate)])                                         #code to ebe edited to check how many and which dates each variable is missing

for(i in 1:length(Sun)){                                                          
  Week_Data = MergedTab[MergedTab$Date <= Sun[i] & MergedTab$Date > (Sun[i]-7),
                        VarsExport]
  
  for (V in VarsExport[-1]){
    a = na.omit(Week_Data[V])
    Mex_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
}

write.csv(Mex_w, "Mexicopaper_Data/Mex_weekly.csv", row.names = F)                                 #Exporting weekly data

# Removing unused variables -----------------------------------------------

rm(Sun,V, VarsExport, MergedTab, Week_Data)
