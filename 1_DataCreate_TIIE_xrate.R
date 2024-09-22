# This file imports TIIE and exchange rate data downloaded from Banxico

TIIE = read_xlsx("SVAR/Mexico/Daily/TIIE_daily.xlsx", range = "A18:C6350")        #importing policy rate and overnight rate downloaded from Banxico
TIIE$Date = as.Date(TIIE$Date)                                                    #converting POSIXct to Date
TIIE[,-1] = lapply(TIIE[,-1], as.numeric)
colnames(TIIE)[-1] = c("Tgt_rate","TIIE")

MXN = read_xlsx("SVAR/Mexico/Daily/MXN_USD_daily.xlsx", range ="A18:B17785")
MXN$Date = as.Date(MXN$Date)                                                      #converting POSIXct to Date
colnames(MXN)[-1] = "MXN_USD"