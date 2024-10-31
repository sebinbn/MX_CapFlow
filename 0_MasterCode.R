# This code replicates the SVAR results in Mexico paper.



# Preliminaries -----------------------------------------------------------

library(readxl)       #for importing data from xlsx
library(ggplot2)      #for plots
library(reshape2)     #for using melt() which converts data to long format convenient for ggplot
library(ggpubr)       #for multiple plots in one figure using ggarrange()
library(zoo)



#setwd("C:/Users/sbnidhir/OneDrive - University Of Houston/Research/MP transmission/Data_Analysis")
#setwd("C:/Users/sebin/OneDrive - University Of Houston/Research/MP transmission/Data_Analysis")
setwd('../')                                                                      #goes up one level to Data_Analysis folder
rm(list = ls())

# Cleaning Data -----------------------------------------------------

# Uses data from Bloomberg in MXYC_1mo30y_PLAST.xlsx (This has last price data at 
# daily frequency) and creates BBYield and  start_date_yield showing dates when 
# data starts.
source("Mexicopaper_analysis/1_DataCreate_BByield.R")

# Uses data from Bloomberg in MXYC_1mo30y_PBAM.xlsx  and creates BBLiq (daily freq)
# Liq_w (weekly frequency) which have bid-ask spreads. 
source("Mexicopaper_analysis/1_DataCreate_BBLiq.R")

# Uses data from Banxico in Mex_GD_ownership.xlsx and creates Own_Data with all
# ownership data, Mex_FO with foreign ownership data. Also, exports Mex_FO. Creates
# start_date_Own showing dates when data starts.
source("Mexicopaper_analysis/1_DataCreate_FO.R")

# Uses data from Banxico in TIIE_daily.xlsx and MXN_USD_daily.xlsx. It creates 
# TIIE and MXN from this.
source("Mexicopaper_analysis/1_DataCreate_TIIE_xrate.R")


# Uses data in EFFR.csv to create EFFR (daily data) and EFFR_w (weekly data)
source("Mexicopaper_analysis/1_DataCreate_EFFR.R")

# Uses data from Banxico in Mex_GD_Maturity.xlsx to create Mex_mat. This is 
# needed only for 1 plot
source("Mexicopaper_analysis/1_DataCreate_Mat.R")

# These various R datatables are merged and one table with necessary variables is
# created for both daily and weekly frequencies as Mex_d and Mex_w. These are also
# exported as Mex_Daily and Mex_Weekly. Weekly data is just the daily data for 
# the last day of a week. This is how weekly data was generated in Bloomberg. So,
# daily data is retrieved from Bloomberg and then used to create the weekly
# frequency data.
source("Mexicopaper_analysis/1_TabCreate_SVAR.R")

# Analysis ----------------------------------------------------------------

# Elasticity for each owner class for few yields are calculated using Own_Data,
# BBYield, TIIE and Mex_w. It returns Tab_Elast_d for daily data and Tab_Elast_w
# for weekly data
#need to adapt this for new Mex_w which has all variables
source("Mexicopaper_analysis/2_Elasticity.R")

# Using Mex_w, ADF test is run on the 4 SVAR variables both at levels and first
# differences. The results are stored in matrix ADF_tab. Returns first differenced
# Mex_w_d used in 2_SVAR.R
source("Mexicopaper_analysis/2_ADFtest.R")

# Runs (1) ARIMAX of 10yr and 1mo yield on Proportion of Foreign Ownership
# (2) a linear regression of BA spread on FO. Uses Mex_W. This suffers from 
# endogeneity.
source("Mexicopaper_analysis/2_ARIMAX_Liq.R")

# Runs aforementioned (1) ARIMAX of 10yr and 1mo yield on Proportion of FO and
# (2) a linear regression of BA spread on FO using EFFR as IV
source("Mexicopaper_analysis/2_EFFR_IV.R")

# SVAR analysis. Uses Mex_W and Mex_d
source("Mexicopaper_analysis/2_SVAR.R")
source("Mexicopaper_analysis/2_SVAR_TIIE.R")



# Creating figures --------------------------------------------------------

source("Mexicopaper_analysis/3_Fig_FO.R")                                       # time plot of Foreign ownership as value and share
FO_value_plot
FO_share_plot

source("Mexicopaper_analysis/3_Fig_Mat.R")                                       # time plot of share of long term debt Foreign ownership as value and share
Mat_plot

source("Mexicopaper_analysis/3_Fig_OwnShare.R")                                       # time plot of share of long term debt Foreign ownership as value and share
OwnShare_plot

source("Mexicopaper_analysis/3_Fig_yields.R")                                   # time plot of yields. creates 3 plots, uses only 1 of them
Yield_plot                                                                          

source("Mexicopaper_analysis/3_Fig_barchart.R")                                   # barchart showing change in proportion
barchart                                                                          # plot doesn't show up if in another code. So, plot is saved as barchart and then called to be displayed here. 

source("Mexicopaper_analysis/3_IRFs.R") 
