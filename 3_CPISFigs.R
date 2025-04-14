# This file loads data from CPIS, subsets mexico data to create two plots

# 1. Plot showing foreign ownership value by country
# 2. Plot showing foreign ownership value by sector
# 3. Plot showing foreign ownership share by sector
# 3. Plot comparing foreign and domestic ownership sector split



# Load CPIS data ------------------------------------------------

# Load CPIS GG data. This dataset was created in code CPIS_GG App creation code.
if(substr(getwd(),10,14) == "sebin"){#if condition determines file location based on which computer I am using
  filepath = "C:/Users/sebin/OneDrive - University Of Houston/Research/International_Finance/"
}else{
  filepath = "C:/Users/sbnidhir/OneDrive - University Of Houston/Research/International_Finance/"
}

CPIS_GGData = readRDS(paste(filepath, "CPIS_GG_App/CPIS_GG.Rds", sep = ""))
#The above data is in wide form, with all countries in one column, and years of 
#data across columns.The table also contains both annual and semi-annual data

# Subsetting Mexico data
MX_GG_Index = CPIS_GGData$Counterpart.Country.Name == "Mexico"  

# Create Country-split data ------------------------------------------------
          
CPIS_GGDataCols= c(1,2,12:74) #extracting country name, code, and yearly data
MX_GG_Index  = MX_GG_Index & 
  CPIS_GGData$Indicator.Code ==  'I_A_D_T_T_BP6_USD' & 
  CPIS_GGData$Counterpart.Sector.Code == 'GG' &
  CPIS_GGData$Sector.Code == 'T'
CPIS_MX_Raw = CPIS_GGData[MX_GG_Index, CPIS_GGDataCols]

# Subsetting semi-annual data (using annual data does not give longer range of data)
CPIS_MX_Raw = CPIS_MX_Raw[CPIS_MX_Raw$X2023S2 != "",] #removing annual data rows                                              
CPIS_MX_Raw = CPIS_MX_Raw[,c(T,T,grepl("S", colnames(CPIS_MX_Raw)[-c(1:2)]) ) ] #removing columns which held the annual data 

# putting subsetted MXN data in long form.
CPIS_MX_long = data.frame(t(CPIS_MX_Raw[,-c(1,2)]))
colnames(CPIS_MX_long) = CPIS_MX_Raw$Country.Name
# creating date column from dates strings
dates = rep("", nrow(CPIS_MX_long) )
sem_indx = substr(rownames(CPIS_MX_long),6,7) == "S2"
dates[sem_indx] = paste(substr(rownames(CPIS_MX_long)[sem_indx],2,5),
                        "-12-31", sep = "")
dates[!sem_indx] = paste(substr(rownames(CPIS_MX_long)[!sem_indx],2,5),
                         "-06-30", sep = "")
CPIS_MX = cbind(Date = as.Date(dates), 
                     data.frame(lapply(CPIS_MX_long, as.numeric)) ) #converting values in characters to numeric



## Creating Others/Total column --------------------------------------------

t5 = order(as.double(CPIS_MX[nrow(CPIS_MX),-1]),decreasing = T)[1:5]          # captures index of top5 values for latest period (2023 Dec).
t5not = colnames(CPIS_MX)[-c(1,t5+1)]
t5 = colnames(CPIS_MX)[t5+1]

CPIS_MX = cbind(CPIS_MX, 
                  Others = rowSums(CPIS_MX[,t5not], na.rm = T),
                  Total = rowSums(CPIS_MX[,-1], na.rm = T))
CPIS_MX = CPIS_MX[CPIS_MX$Total != 0,]                            # removing rows with no data

CPIS_MX_filled = CPIS_MX
CPIS_MX_filled[t5] = na.locf(CPIS_MX[t5],fromLast = T) #carrying back next obs to fill initial NAs in top 5


# Create Sector-split data ------------------------------------------------

CPIS_GGDataCols= c(1,2,9,10,12:74) #extracting country name & code, sector name & code,and yearly data

MX_GG_Index  = MX_GG_Index & 
  CPIS_GGData$Indicator.Code ==  'I_A_D_T_T_BP6_USD' & 
  CPIS_GGData$Counterpart.Sector.Code == 'GG'
CPIS_MX_Raw = CPIS_GGData[MX_GG_Index, CPIS_GGDataCols]

# Subsetting semi-annual data (using annual data does not give longer range of data)
CPIS_MX_Raw = CPIS_MX_Raw[CPIS_MX_Raw$X2023S2 != "",] #removing annual data rows                                              
CPIS_MX_Raw = CPIS_MX_Raw[,c(T,T,grepl("S", colnames(CPIS_MX_Raw)[-c(1:2)]) ) ] #removing columns which held the annual data 

CPIS_MX_Raw[,5:43] = data.frame(lapply(CPIS_MX_Raw[,5:43], as.numeric))
colnames(CPIS_MX_Raw)
# USA is missing sector classification for 2016S2 - 2018S1. The USA total in 
# these years is allocated to each sector in the proportion of 2016S1.
sec_propn = CPIS_MX_Raw[CPIS_MX_Raw$Country.Name == "United.States" & 
              CPIS_MX_Raw$Sector.Code != "T", "X2016S1"]/
  CPIS_MX_Raw[CPIS_MX_Raw$Country.Name == "United.States" & 
                CPIS_MX_Raw$Sector.Code == "T", "X2016S1"]
Tot = as.double(CPIS_MX_Raw[CPIS_MX_Raw$Country.Name == "United.States" & 
                              CPIS_MX_Raw$Sector.Code == "T",
                            c("X2016S2","X2017S1","X2017S2","X2018S1")])

CPIS_MX_Raw[CPIS_MX_Raw$Country.Name == "United.States" & 
              CPIS_MX_Raw$Sector.Code != "T",
            c("X2016S2","X2017S1","X2017S2","X2018S1")] = sec_propn %*% t(Tot)


# for each sector code, I sum across countries, transpose the result and bind it with date column below
CPIS_MX_sec = cbind(Date = as.Date(dates),
                    data.frame(t(rowsum(CPIS_MX_Raw[,5:43], 
                                        group = CPIS_MX_Raw$Sector.Code,
                                        na.rm = T))))

# There are 17 sectors according to CPIS metadata. CPIS_GGData has only 12. Of these
# 12, some are composite of others. So total is lower than the sum of 11 categories.

#CPIS_MX_sec$T - rowSums(CPIS_MX_sec[,2:12])

# Excluding composite categories of NHN and OFT, remaining is added as 'Unclassified' - UC
CPIS_MX_sec$UC = CPIS_MX_sec$T - rowSums(CPIS_MX_sec[,c(2:7,9,10,12)])  

CPIS_MX_sec$Other = rowSums(CPIS_MX_sec[,c("CB","GG", "NP","HH")])

CPIS_MX_sec = CPIS_MX_sec[CPIS_MX_sec$T != 0,]                            # removing rows with no data

# Removing excess data/variables ------------------------------------------

rm(CPIS_GGData, CPIS_MX_Raw, CPIS_MX_long, MX_GG_Index, sem_indx, CPIS_GGDataCols)


# Creating Plots ----------------------------------------------------------


## Plot 1 : FO split by country --------------------------------------

MX_long = melt(CPIS_MX_filled[,c('Date',t5,'Others')],id.vars = "Date")  
cntry_plot = ggplot(data = MX_long, 
                      aes(x = Date, y = value/1000000000, color = variable)) +
  geom_area(aes(fill = variable))+
  scale_fill_brewer(palette = "Set2") +
  guides(color = "none") +
  scale_x_date(expand = c(0, 0))+
    labs(y = 'Billions of USD', x = element_blank(), title = "Country-split of Mexican FO")+ 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          legend.title = element_blank(),legend.text = element_text(size = 14))
cntry_plot

## Plot 2 : FO split by country --------------------------------------

MX_sec_long = melt(CPIS_MX_sec[,!colnames(CPIS_MX_sec) %in% 
                                 c("CB","GG", "NP","HH", "NHN","OFT","T")],
                   id.vars = "Date")  
sec_plot = ggplot(data = MX_sec_long, 
                    aes(x = Date, y = value/1000000000, color = variable)) +
  geom_area(aes(fill = variable))+
  scale_fill_brewer(palette = "Set3") +
  guides(color = "none") +
  scale_x_date(expand = c(0, 0))+
  labs(y = 'Billions of USD', x = element_blank(), title = "Sector-split of Mexican FO")+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14))
sec_plot
