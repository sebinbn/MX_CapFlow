# This file explores EFFR data downloaded from FRED and creates weekly data.
# creates two files EFFR (dailydata) and EFFR_w (weekly data)

EFFR = read.csv('EFFR.csv')
EFFR$DATE = as.Date(EFFR$DATE)
EFFR$EFFR = as.numeric(EFFR$EFFR)

IIP = read.csv('INDPRO.csv')
IIP$DATE = as.Date(IIP$DATE)

# Creating Weekly data ----------------------------------------------------

Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

EFFR_w = data.frame(Date = Sun,
                    Values = matrix(NaN, length(Sun),1 ) )                #Initiating a dataframe
colnames(EFFR_w)[-1] = 'EFFR'

for(i in 1:length(Sun)){                                                          
  Week_Data = EFFR[EFFR$DATE <= Sun[i] & EFFR$DATE > (Sun[i]-7), ]
  a = na.omit(Week_Data[,2])
  EFFR_w[i,2] = if(length(a) != 0) tail(a,1) else NA
} 

# Creating Monthly data ----------------------------------------------------
Month = seq(as.Date("2006-02-01"), as.Date("2024-01-01"), by = "month")-1          #creating vector of last day of months.

EFFR_m = data.frame(Date = Month, Values = matrix(NaN, length(Month),1) )
colnames(EFFR_m)[-1] = 'EFFR'

for(i in 1:length(Month)){                                                          
  Month_Data = EFFR[EFFR$DATE <= Month[i] & EFFR$DATE > (Month[i]-10), ] #here 10 is arbitrary. It has to be a number large enough so that there are non NAs
  a = na.omit(Month_Data[,2])
  EFFR_m[i,2] = if(length(a) != 0) tail(a,1) else NA
}

# Removing unnecessary variables ------------------------------------------


rm(Sun,Month, Week_Data,Month_Data, a,i)
