# This file compares daily data on exchange rate obtained form Bloomberg and
# Banxico and compares the values. Given high correlation and similar values, using
# either seems fine. I choose to use the data from Banxico so as to limit use of 
# Bloomberg.


# Importing xrate from BB -------------------------------------------------

MXN_BB = read_xlsx("BB_Data/USD_MXN/USD_MXN_1990_2024_daily.xlsx",
                   range = "A7:B8965")
MXN_BB$Date = as.Date(MXN_BB$Date)                                                      #converting POSIXct to Date
colnames(MXN_BB)[-1] = "USD_MXN"             
MXN_BB = MXN_BB[order(MXN_BB$Date),]


# Importing xrate from Banxico --------------------------------------------

MXN = read_xlsx("SVAR/Mexico/Daily/MXN_USD_daily.xlsx", range ="A18:B17785")
MXN$Date = as.Date(MXN$Date) #converting POSIXct to Date


# Comparing both data -----------------------------------------------------

xrate = merge(MXN_BB, MXN)
xrate$diff = xrate$USD_MXN - xrate$SF63528
mean(xrate$diff)                                                                  #both data sources are very identical
which(xrate$diff == max(xrate$diff)) 
cor(xrate$USD_MXN,xrate$SF63528)

rm(MXN_BB)                                                                        #We choose to use Banxico data
