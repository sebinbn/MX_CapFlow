# This file loads data from CPIS, subsets mexico data to create two plots

# 1. Plot showing foregin ownership split by country
# 2. Plot showing foregin ownership split by sector
# 3. Plot comparing foreign and domestic ownership sector split


# Load CPIS GG data. This dataset was created in code CPIS_GG App creation code.
if(substr(getwd(),10,14) == "sebin"){
  filepath = "C:/Users/sebin/OneDrive - University Of Houston/Research/International_Finance/"
}else{
  filepath = "C:/Users/sbnidhir/OneDrive - University Of Houston/Research/International_Finance/"
}

CPIS_GGData = readRDS(paste(filepath, "CPIS_GG_App/CPIS_GG.Rds", sep = ""))
#The above data is in wide form, with all countries in one column, and years of 
#data across columns.The table also contains both annual and semi-annual data

# Subsetting Mexico data
MX_GG_Index = CPIS_GGData$Counterpart.Country.Name == "Mexico"                    
CPIS_GGDataCols= c(1,2,12:74) #extracting country name, code, and yearly data
MX_GG_Index  = MX_GG_Index & 
  CPIS_GGData$Indicator.Code ==  'I_A_D_T_T_BP6_USD' & 
  CPIS_GGData$Counterpart.Sector.Code == 'GG' &
  CPIS_GGData$Sector.Code == 'T'
CPIS_MX = CPIS_GGData[MX_GG_Index, CPIS_GGDataCols]

# Subsetting semi-annual data (using annual data does not give longer range of data)
CPIS_MX = CPIS_MX[CPIS_MX$X2023S2 != "",] #removing annual data rows                                              
CPIS_MX = CPIS_MX[,c(T,T,grepl("S", colnames(CPIS_MX)[-c(1:2)]) ) ] #removing columns which held the annual data 

# putting subsetted MXN data in long form.
CPIS_MX_long = data.frame(t(CPIS_MX[,-c(1,2)]))
colnames(CPIS_MX_long) = CPIS_MX$Country.Name
# creating date column from dates strings
dates = rep("", nrow(CPIS_MX_long) )
sem_indx = substr(rownames(CPIS_MX_long),6,7) == "S2"
dates[sem_indx] = paste(substr(rownames(CPIS_MX_long)[sem_indx],2,5),
                        "-12-31", sep = "")
dates[!sem_indx] = paste(substr(rownames(CPIS_MX_long)[!sem_indx],2,5),
                         "-06-30", sep = "")
CPIS_MX_Data = cbind(Date = as.Date(dates), 
                     data.frame(lapply(CPIS_MX_long, as.numeric)) ) #converting values in characters to numeric

t10 = order(as.double(cntryData[nrow(cntryData),-1]),decreasing = T)[1:10]         # captures index of top10 values.
notTop10 = colnames(cntryData)[-c(1,t10+1)]


cntryLiab = cbind(cntryData, 
                  Others = rowSums(cntryData[,notTop10], na.rm = T),
                  Total = rowSums(cntryData[,-1], na.rm = T))
cntryLiab = cntryLiab[cntryLiab$Total != 0,]                           # removing rows with no data
Top10 = colnames(cntryData)[t10+1]

# Plot 3 : Comparing Domestic vs Foreign --------------------------------------