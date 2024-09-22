# This code uses imf.data package in R which uses API to query the IMF Database.
# The code queries CPIS data


library(imf.data)
library(ggplot2)
library(ggpubr) #for multiple ggplots in one
library(reshape2) #for melt
library(zoo) #for na.approx

# Getting CPIS data structure ---------------------------------------

CPIS = load_datasets("CPIS")
CPIS$dimensions$ref_sector
indic = CPIS$dimensions$indicator
countries = CPIS$dimensions$counterpart_area
sector = CPIS$dimensions$counterpart_sector

# Getting assets in MX using API -----------------------------------------

# Learning: Liabilities tables are empty. So, use assets and put counterpart as 
# Mexico. World is empty but there are individual country data. Secure all 
# available data and then add them up. 

# Key 
# *****************
#
# B - biannual freq, GG - General govt, T - Total
# I_A_D_T_T_BP6_USD - Assets, Debt Securities, BPM6, US Dollars
# I_A_D_L_T_BP6_USD - Assets, Debt Securities, Long-term, BPM6, US Dollars
# I_A_D_S_T_BP6_USD - Assets, Debt Securities, Short-term, BPM6, US Dollars
# I_A_D_T_USD_BP6_USD - Assets, Debt Securities, Denominated in US Dollars, BPM6, US Dollars
# I_A_D_T_O_BP6_USD - Assets, Debt Securities, Denominated in Other Currencies, BPM6, US Dollars
# I_A_D_L_USD_BP6_USD - Assets, Debt Securities, Long-term, Denominated in US Dollars, BPM6, US Dollars
# I_A_D_S_USD_BP6_USD - Assets, Debt Securities, Short-term, Denominated in US Dollars, BPM6, US Dollars
# I_A_D_L_O_BP6_USD - Assets, Debt Securities, Long-term, Denominated in Other Currencies, BPM6, US Dollars
# I_A_D_S_O_BP6_USD - Assets, Debt Securities, Short-term, Denominated in Other Currencies, BPM6, US Dollars


# Defining function -------------------------------------------------------



# US investment in varied Mexican debt ------------------------------------

US_MX = CPIS$get_series('B', 'US', 'I_A_D_T_T_BP6_USD', 'T',sector$Value, 'MX') #Biannual freq, assets of US, in Debt instruments, from all sectors into General Government and total, of Mexico
US_MX_df = US_MX
US_MX_df[-1] = lapply(US_MX[-1], as.numeric)                                    #values in Millions USD

dates = rep("", nrow(US_MX) )                                                   #creating dates vector for above table
sem_indx = substr(US_MX$TIME_PERIOD,6,7) == "B2"
dates[sem_indx] = paste(substr(US_MX$TIME_PERIOD[sem_indx],1,4),
                                "-12-31", sep = "")
dates[!sem_indx] = paste(substr(US_MX$TIME_PERIOD[!sem_indx],1,4),
                        "-06-30", sep = "")
US_MX_df$TIME_PERIOD = as.Date(dates)
colnames(US_MX_df)[1] = "Date"
# Defining function to extract counterpart sector from column names returned by 
# get_series of imf.data
CsecExtract <- function(x){ substr(x, 26, nchar(x) - 3) }
colnames(US_MX_df)[-1] = CsecExtract(colnames(US_MX)[-1])

US_MX_long = melt(US_MX_df[,c("Date","OFT","FC","DC","NHN","GG")],
                  id.vars = "Date")                                               #OFT = OFX, ODX +CB = DC, since CB =0 here, ODX = DC   
US_MX_plot = ggplot(data = US_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  labs(y = 'Billions of USD', x = element_blank())+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14))
US_MX_plot


# Various countries in Mexican govt debt ----------------------------------

#WO_MXtest = CPIS$get_series('B', 'CN', 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')
WO_MX1 = CPIS$get_series('B', countries$Value[1:50], 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')
WO_MX2 = CPIS$get_series('B', countries$Value[51:100], 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')
WO_MX3 = CPIS$get_series('B', countries$Value[101:150], 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')
WO_MX4 = CPIS$get_series('B', countries$Value[151:200], 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')
WO_MX5 = CPIS$get_series('B', countries$Value[201:249], 'I_A_D_T_T_BP6_USD', 'T','GG', 'MX')

WO_MX = merge(WO_MX1, WO_MX2, by = 'TIME_PERIOD')
WO_MX = merge(WO_MX, WO_MX3, by = 'TIME_PERIOD')
WO_MX = merge(WO_MX, WO_MX4, by = 'TIME_PERIOD')
WO_MX = merge(WO_MX, WO_MX5, by = 'TIME_PERIOD')

WO_MX_df = WO_MX
WO_MX_df[-1] = lapply(WO_MX[-1], as.numeric)
WO_MX_df[1] = seq(as.Date("2013-06-30"), as.Date("2023-06-30"), by = '6 month')
colnames(WO_MX_df)[1] = "Date"
colnames(WO_MX_df)[-1] = substr(colnames(WO_MX)[-1],3,4)

#Having graph with all countries is confusing, so countries with small flows are
# clubbed together
low_country = colnames(WO_MX_df)[WO_MX_df[nrow(WO_MX_df),] < 500 | 
                                   is.na(WO_MX_df[nrow(WO_MX_df),])]
WO_MX_df$Others = rowSums(WO_MX_df[,low_country], na.rm = T)

WO_MX_long = melt(WO_MX_df[,c('Date','ES','IT','US','FR','DE','Others')],
                  id.vars = "Date")                                               
WO_MX_plot = ggplot(data = WO_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  labs(y = 'Billions of USD', x = element_blank())+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14))
WO_MX_plot


# Country-sector into Mex GG debt -----------------------------------------

country = colnames(WO_MX_df)[!colnames(WO_MX_df) %in% c('Date', 'Others')]
WO_S_MX = CPIS$get_series('B',c('ES','IT','US','FR','DE'), 'I_A_D_T_T_BP6_USD',
                          sector$Value,'GG', 'MX')                                #when all countries used, there is an error

WO_S_MX_df = WO_S_MX
WO_S_MX_df[-1] = lapply(WO_S_MX[-1], as.numeric)
WO_S_MX_df[1] = seq(as.Date("2013-06-30"), as.Date("2023-06-30"), by = '6 month')
colnames(WO_S_MX_df)[1] = "Date"
# Defining function to extract origin sector 
OsecExtract <- function(x){ substr(x, 24, nchar(x) - 6)
}
colnames(WO_S_MX_df)[-1] = paste(substr(colnames(WO_S_MX)[-1],3,4),
                                OsecExtract(colnames(WO_S_MX)[-1]), sep = "_")

WO_S_MX_df = WO_S_MX_df[,c('Date',sort(colnames(WO_S_MX_df)[-1]))]              #sorting column names so that countries are together



# World sectors -----------------------------------------------------------
# here, world is the top 5 countries investing in Mexican debt. I find that USA
# has missing sector level values from Dec 2016 until June 2018. But these dates
# have the total US investment. So, missing values are interpolated using proportion
# of total in each sector.



## Interpolating US values -------------------------------------------------

US_props = cbind(Date = WO_S_MX_df[,'Date'],
                 WO_S_MX_df[,substr(colnames(WO_S_MX_df),1,2) == 'US']/WO_S_MX_df[,'US_T'])
US_props[4:21,-1] = na.approx(US_props[-1]) #improve  later to use dates and not row numbers. Is it worth it?
WO_S_MX_df[8:11,substr(colnames(WO_S_MX_df),1,2) == 'US'] = 
  US_props[8:11,-1] * WO_S_MX_df[8:11,'US_T']

## Calculating sector totals -----------------------------------------------

substrStart = function(x,n){substr(x,n, nchar(x))}                                #function that subsets from n to the end
for (sec in sector$Value){                                                        # calculating sector totals across the 5 economies
WO_S_MX_df[,sec] = rowSums(WO_S_MX_df[,sec==substrStart(colnames(WO_S_MX_df),4)], 
                           na.rm = T)
}


## Plotting ----------------------------------------------------------------

WO_S_MX_long = melt(WO_S_MX_df[,c('Date','NP','GG','HH','MMF','ODX','NFC','IPF','OFX')],
                  id.vars = "Date")                                               
WO_S_MX_plot = ggplot(data = WO_S_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  labs(y = 'Billions of USD', x = element_blank())+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14))
WO_S_MX_plot

#Country wise sectoral decomposition -----------------------------------------

## USA ---------------------------------------------------------------------

US_S_MX_long = melt(WO_S_MX_df[,c('Date',paste('US',c('NP','GG','HH','MMF',
                                                      'ODX','NFC','IPF','OFX'), 
                                               sep = '_' ))],
                    id.vars = 'Date')
US_S_MX_plot = ggplot(data = US_S_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  scale_fill_discrete(labels = c('NP','GG','HH','MMF','ODX','NFC','IPF','OFX'))+
  labs(y = 'Billions of USD', x = element_blank(), title = 'USA')+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))
US_S_MX_plot

## Germany ---------------------------------------------------------------------

DE_S_MX_long = melt(WO_S_MX_df[,c('Date',paste('DE',c('NP','GG','HH','MMF',
                                                      'ODX','NFC','IPF','OFX'),
                                               sep = '_' ))],
                    id.vars = 'Date')
DE_S_MX_plot = ggplot(data = DE_S_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  scale_fill_discrete(labels = c('NP','GG','HH','MMF','ODX','NFC','IPF','OFX'))+
  labs(y = 'Billions of USD', x = element_blank(), title = 'Germany')+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))
DE_S_MX_plot

## Italy ---------------------------------------------------------------------

IT_S_MX_long = melt(WO_S_MX_df[,c('Date',paste('IT',c('NP','GG','HH','MMF',
                                                      'ODX','NFC','IPF','OFX'),
                                               sep = '_' ))],
                    id.vars = 'Date')
IT_S_MX_plot = ggplot(data = IT_S_MX_long, aes(x = Date, y = value/1000, color = variable)) +
  geom_area(aes(fill = variable))+
  guides(color = "none") +
  scale_fill_discrete(labels = c('NP','GG','HH','MMF','ODX','NFC','IPF','OFX'))+
  labs(y = 'Billions of USD', x = element_blank(), title = 'Italy')+ 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))
IT_S_MX_plot


# Collating country plots -------------------------------------------------

Country_S_MX_plot = ggarrange(US_S_MX_plot, DE_S_MX_plot, IT_S_MX_plot, 
                              common.legend = T, legend = 'bottom')
Country_S_MX_plot
# Creating dataframes -----------------------------------------------------

Mex_L$B.W00.I_A_T_T_T_BP6_USD.T.T.MX  = as.numeric(Mex_L$B.W00.I_A_T_T_T_BP6_USD.T.T.MX)
Mex_L_Date = c(as.Date("1997-12-31"),
               seq(as.Date("2001-12-31"), as.Date("2012-12-31"), by = 'year'),
               seq(as.Date("2013-06-30"), as.Date("2023-06-30"), by = '6 month'))
Mex_L$TIME_PERIOD = Mex_L_Date
Mex_L = data.frame(Date = Mex_L$TIME_PERIOD, Total = Mex_L$B.W00.I_A_T_T_T_BP6_USD.T.T.MX)

Mex_GG[,-1] = lapply(Mex_GG[-1], as.numeric)
Mex_GG[1] = Mex_L_Date

US_GG$B.MX.I_A_D_T_T_BP6_USD.T.T.US = as.numeric(US_GG$B.MX.I_A_D_T_T_BP6_USD.T.T.US)
US_GG$TIME_PERIOD = c(seq(as.Date("2003-12-31"), as.Date("2012-12-31"), by = 'year'),
                      seq(as.Date("2013-06-30"), as.Date("2023-06-30"), by = '6 month'))

# Plotting ----------------------------------------------------------------

ggplot(Mex_L, aes(x = 'TIME_PERIOD', y = 'B.W00.I_A_T_T_T_BP6_USD.T.T.MX'))+
       geom_line(linewidth = 1.2, color = "blue")

ggplot(Mex_L, aes(x = Date, y = Total)) +
  geom_line(linewidth = 1.2, color = "blue")

ggplot(Mex_GG, aes(x = TIME_PERIOD, y = B.US.I_A_D_T_T_BP6_USD.T.GG.MX)) +
  geom_line(linewidth = 1.2, color = "blue")

ggplot(US_GG, aes(x = TIME_PERIOD, y = B.MX.I_A_D_T_T_BP6_USD.T.T.US)) +
  geom_line(linewidth = 1.2, color = "blue")



# Comparing Banxico and CPIS ----------------------------------------------


