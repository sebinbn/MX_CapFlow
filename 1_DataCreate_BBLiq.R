#This R file does the following

# 1. Import mid, ask, bid data downloaded from Bloomberg and converts to date 
# and numeric.
# 2. It finds the date from when each variable's data is available

# Data Import and Cleaning ------------------------------------------------


## Importing and Cleaning yield data from BB -------------------------------

# Importing mid,ask,bid data. The column names get repeated for each and so these
# are renamed to indicate whether it is mid, ask or bid. Similar to BBYield, data
# has all dates and for days when market was closed, the value is NA.It is ordered
# descending by dates and is changed to ascending.
BBLiq = read_xlsx("BB_Data/MXYC_1mo30y_PBAM.xlsx", range = "A2:BC8891")
colnames(BBLiq)[1] = 'Date'
colnames(BBLiq)[-1] = c(
  paste('M', substr(colnames(BBLiq)[2:14], 5,7), sep = "_"),
  paste('M', substr(colnames(BBLiq)[15:19], 3,5), sep = "_"),
  paste('A', substr(colnames(BBLiq)[20:32], 5,7), sep = "_"),
  paste('A', substr(colnames(BBLiq)[33:37], 3,5), sep = "_"),
  paste('B', substr(colnames(BBLiq)[38:50], 5,7), sep = "_"),
  paste('B', substr(colnames(BBLiq)[51:55], 3,5), sep = "_") )

BBLiq$Date = as.Date(BBLiq$Date) #converting POSIXct to Date
BBLiq[,-1] = lapply(BBLiq[,-1], as.numeric)

BBLiq = BBLiq[order(BBLiq$Date),]

# Creating Weekly data ----------------------------------------------------

VarsExport = c("TBA", "TBF", "01Y","02Y", "05Y", "10Y", "30Y")
VarsExport = c(paste('B', VarsExport, sep = "_" ), 
               paste('A', VarsExport, sep = "_" ))

Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

Liq_w = data.frame(Date = Sun,
                     Values = matrix(NaN, length(Sun),length(VarsExport) ) )                #Initiating a dataframe
colnames(Liq_w)[-1] = VarsExport

for(i in 1:length(Sun)){                                                          
  Week_Data = BBLiq[BBLiq$Date <= Sun[i] & BBLiq$Date > (Sun[i]-7),
                        c('Date',VarsExport) ]
  
  for (V in VarsExport){
    a = na.omit(Week_Data[V])
    Liq_w[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
} 

# Creating Monthly data ----------------------------------------------------

Month = seq(as.Date("2006-02-01"), as.Date("2024-01-01"), by = "month")-1          #creating vector of last day of months.
Liq_m = data.frame(Date = Month,
                   Values = matrix(NaN, length(Month),length(VarsExport) ) )                #Initiating a dataframe
colnames(Liq_m)[-1] = VarsExport

for(i in 1:length(Month)){                                                          
  Month_Data = BBLiq[BBLiq$Date <= Month[i] & BBLiq$Date > (Month[i]-10),
                    c('Date',VarsExport) ]
  
  for (V in VarsExport){
    a = na.omit(Month_Data[V])
    Liq_m[i, V] = if(nrow(a) != 0) tail(a,1) else NA
  }
} 


# Calculating bid-ask spread ----------------------------------------------

NewCols = paste('BA' , c("TBA", "TBF", "01Y","02Y", "05Y", "10Y", "30Y") , sep= "_") 
Liq_w[NewCols] = Liq_w[,2:8] - Liq_w[9:15]
Liq_m[NewCols] = Liq_m[,2:8] - Liq_m[9:15]

colSums(is.na(Liq_w)) #seeing number of missing values


# Removing unnecessary variables ------------------------------------------

rm(Sun, Month, Week_Data, Month_Data, VarsExport, V, i)
