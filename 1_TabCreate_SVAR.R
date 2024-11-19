# This file merges the various data to create a single table to be used in SVAR.
# It creates the daily frequency table and then the weekly frequency table using
# end of week daily data.


# Creating Merged table ---------------------------------------------------

MergedTab  = merge(BBYield, Mex_FO)
MergedTab = merge(MergedTab, MXN)
MergedTab = merge(MergedTab, TIIE, all= T)


# Creating Daily Data  ----------------------------------------------------

Mex_d = MergedTab[MergedTab$Date <= as.Date("2023-12-31") & 
                    MergedTab$Date >= as.Date("2006-01-01"), ]            #Creating subset of  daily data for SVAR required variables from 2006
                 
write.csv(Mex_d, "Mexicopaper_Data/Mex_Daily_MH.csv", row.names = F)                                 #Exporting daily data

# Creating Weekly Data  ---------------------------------------------------

# LOGIC
# Creating weekly data as the data on the latest trading day of the week. First,
# all sunday dates are created and this is used to extract the weekly table. From
# the weekly table, the last value is picked of each variable and stored in Mex_w.
# Note: TIIE starts only from 2006-01-02 and so has an NA for 2006-01-01 which is 
# a Sunday


Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

Mex_w = data.frame(Date = Sun, Values = matrix(NaN, length(Sun),
                                               ncol(MergedTab) -1) )                #Initiating a dataframe
colnames(Mex_w)[-1] = colnames(MergedTab)[-1]

#colMeans(is.na(Mex_d))                                                            #checking which variables have missing values - only MTBA and GMXN10y have missing values
#length(Mex_d$Date[is.na(Mex_d$Tgt_rate)])                                         #code to be edited to check how many and which dates each variable is missing

for(i in 1:length(Sun)){                                                          
  Week_Data = MergedTab[MergedTab$Date <= Sun[i] & MergedTab$Date > (Sun[i]-7),]
  
  for (V in names(MergedTab)[-1]){
    a = na.omit(Week_Data[V])
    Mex_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
}
colMeans(is.na(Mex_w))

# Removing NAs between 2010 to 2019. Only this period is considered because that is 
# what is needed to be interpolated for SVAR analysis period. There are some very
# large NA gaps in other periods before 2010 and after 2019 which are best left as is.

# the dates of 2017-04-30 and 2009-11-01 are decided based on trying out multiple
# dates. If the beginning/ending of the data has NAs, they are retained by na.approx

# 4yr, 7yr and 9yr has a large gap which requires using 2017-04-30
Index = Mex_w$Date <= as.Date("2019-12-31") & Mex_w$Date >= as.Date("2009-11-01")
Mex_w[Index,-1] = na.approx(Mex_w[Index,-1], na.rm = F)
write.csv(Mex_w, "Mexicopaper_Data/Mex_weekly.csv", row.names = F)                                 #Exporting weekly data


# Creating Monthly Data  ---------------------------------------------------

# LOGIC
# Creating monthly data as the data on the latest trading day of the month. First,
# all sunday dates are created and this is used to extract the weekly table. From
# the weekly table, the last value is picked of each variable and stored in Mex_w.
# Note: TIIE starts only from 2006-01-02 and so has an NA for 2006-01-01 which is 
# a Sunday


Month = seq(as.Date("2006-02-01"), as.Date("2024-01-01"), by = "month")-1          #creating vector of last day of months.

Mex_m = data.frame(Date = Month, Values = matrix(NaN, length(Month),
                                               ncol(MergedTab) -1) )                #Initiating a dataframe
colnames(Mex_m)[-1] = colnames(MergedTab)[-1]

for(i in 1:length(Month)){                                                          
  Month_Data = MergedTab[MergedTab$Date <= Month[i] & MergedTab$Date > (Month[i]-20),] #here 20 is arbitrary. It has to be a number large enough so that there are non NAs
  
  for (V in names(MergedTab)[-1]){
    a = na.omit(Month_Data[V])
    Mex_m[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
}

Index = Mex_m$Date <= as.Date("2022-12-31")
colMeans(is.na(Mex_m[Index,]))
Mex_m[Index,"GMXN10Y"] = na.approx(Mex_m[Index,"GMXN10Y"], na.rm = F)

# Removing unused variables -----------------------------------------------

rm(Sun,V, MergedTab, Week_Data, Index, Month, Month_Data)
