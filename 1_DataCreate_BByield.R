#This R file does the following

# 1. Import data downloaded from Bloomberg and converts to date and numeric.
# 2. It finds the date from when each variable's data is available

# Data Import and Cleaning ------------------------------------------------


## Importing and Cleaning yield data from BB -------------------------------

#Importing last price data
BBYield = read_xlsx("BB_Data/MXYC_1mo30y_PLAST.xlsx", range = "A2:S8891")
BBYield$Date = as.Date(BBYield$Date) #converting POSIXct to Date
BBYield[,-1] = lapply(BBYield[,-1], as.numeric)
colnames(BBYield)[-1] = c(substr(colnames(BBYield)[2:14],1,7),
                            substr(colnames(BBYield)[15:19],1,5))               #renaming with shorter names
BBYield = BBYield[order(BBYield$Date),]
#The data has all dates and for days when market was closed, the value is NA.


# Data exploration (optional) ---------------------------------------------

## Find starting date of available yield data ------------------------------
a = !is.na(BBYield[,-1])
first_value = NaN
for (i in 1:(ncol(BBYield)-1)){
  first_value[i] = which(a[,i] == T)[1]                                           #this picks the row number when the first non NA value appears in each column
}

start_date_yield = data.frame(Var = colnames(BBYield)[-1], 
                   Start = BBYield$Date[first_value] )
rm(list = c('a', 'first_value'))


