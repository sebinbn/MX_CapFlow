#This R file does the following

# 1. Import bid, ask, mid data downloaded from Bloomberg and converts to date 
# and numeric.
# 2. It finds the date from when each variable's data is available

# Data Import and Cleaning ------------------------------------------------


## Importing and Cleaning yield data from BB -------------------------------

# Importing mid,ask,bid data. The column names get repeated for each and so these
# are renamed to indicate whether it is mid, ask or bid. Similar to BBYield, data
# has all dates and for days when market was closed, the value is NA.It is ordered
# descending by dates and is changed to ascending.
BBVol = read_xlsx("BB_Data/MXYC_1mo30y_PBAM.xlsx", range = "A2:BC8891")
colnames(BBVol)[1] = 'Date'
colnames(BBVol)[-1] = c(
  paste('M', substr(colnames(BBVol)[2:14], 5,8), sep = "_"),
  paste('M', substr(colnames(BBVol)[15:19], 3,5), sep = "_"),
  paste('A', substr(colnames(BBVol)[20:32], 5,8), sep = "_"),
  paste('A', substr(colnames(BBVol)[33:37], 3,5), sep = "_"),
  paste('B', substr(colnames(BBVol)[38:50], 5,8), sep = "_"),
  paste('B', substr(colnames(BBVol)[51:55], 3,5), sep = "_") )

BBVol$Date = as.Date(BBVol$Date) #converting POSIXct to Date
BBVol[,-1] = lapply(BBVol[,-1], as.numeric)

BBVol = BBVol[order(BBVol$Date),]

# Creating Weekly data ----------------------------------------------------




Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

Mex_w = data.frame(Date = Sun, Values = matrix(NA, length(Sun),
                                               length(VarsExport) -1) )                #Initiating a dataframe
colnames(Mex_w) = VarsExport

colMeans(is.na(Mex_d))                                                            #checking which variables have missing values - only MTBA and GMXN10y have missing values
length(Mex_d$Date[is.na(Mex_d$Tgt_rate)])                                         #code to be edited to check how many and which dates each variable is missing

for(i in 1:length(Sun)){                                                          
  Week_Data = MergedTab[MergedTab$Date <= Sun[i] & MergedTab$Date > (Sun[i]-7),
                        VarsExport]
  
  for (V in VarsExport[-1]){
    a = na.omit(Week_Data[V])
    Mex_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
} 

