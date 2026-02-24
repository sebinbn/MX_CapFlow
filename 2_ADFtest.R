# Implemented:
# 1. Daily data for TvVAR is created for every weeekday using Mex_d, missing data
#    is interpolated.The daily data so created is stored in Mex_d_TV and its first
#    difference in Mex_d_TV_diff.
# 2. ADF test is run on levels and differences for the newly created daily data.




# Running ADF Analysis on daily  data -------------------------------------

# Creating DataTable for Regression ---------------------------------------


# Daily data is available in Mex_d for all variables.

# I first scroll through the daily data to find where are a lot of continuous NAs,
# and subset the data to avoid these.
# GMXN10Y is missing in 2007 and upto 15-02-2008. Subset data to start from March 2008.
# MPTBA only available until 2023 MArch. subset upto 31-12-2022.

# A series of weekdays are created and then populated. To get a VAR lag order that is meaningful,
# say 5 for 5 weekdays, it is better not to remove NAs that also would remove market holidays.
# Removing NAs was tried initially, but decided to go with former method as VARselect showed 5 for
# most combinations, but higher lags for some other 3 variable combinations.

all_days <- seq.Date(from = as.Date("2008-03-01"), to = as.Date("2022-12-31"),
                     by = "day")
x <- data.frame(Date = all_days[!(weekdays(all_days) %in% c("Saturday", "Sunday"))] )
Mex_d_TV = merge(x, Mex_d, by.x = "Date", all.x = T)

Vars_ADF = c("MPTBA","MPTBC","MPTBF", "MPTBI", "GMXN10Y","GMXN30Y", "TIIE", "MXN_USD",
             "F_Own", "F_Own_p")
Mex_d_TV[Vars_ADF] = na.approx(Mex_d_TV[Vars_ADF], na.rm = F)

# following code is modified and run thrice for each of the variables
colSums(apply(Mex_d_TV,2, FUN = is.na))

# #Now, remove rows where all three interest rates are missing. These are presumably market holidays.
# Mex_d_TV = Mex_d_TV[!(is.na(Mex_d_TV$GMXN10Y) & is.na(Mex_d_TV$GMXN30Y) & 
#                         is.na(Mex_d_TV$MPTBA)),]
# 
# x = Mex_d_TV[is.na(Mex_d_TV$GMXN10Y),c("Date","MPTBA", "GMXN10Y","GMXN30Y")]
# weekdays(x$Date) #these are not weekends for any of the 3
# rm(x)
# #Now there are 12 GMXN10Y and GMXN30Y missing and 65 MPTBA missing.None of these
# # are weekends. Of the 12, 11 are the same days when both 10 and 30 are missing. 
# # So, dates where GMXN10Y and GMNX30Y are missing are dropped. The remaining MPTBA
# # missing are filled in with linear interpolation.
# 
# Mex_d_TV = Mex_d_TV[!(is.na(Mex_d_TV$GMXN10Y) | is.na(Mex_d_TV$GMXN30Y)),]
# Mex_d_TV$MPTBA = na.approx(Mex_d_TV$MPTBA,na.rm = F)


# Running ADF Test --------------------------------------------------------

Mex_d_TV_diff = lapply(Mex_d_TV[,Vars_ADF ], diff)                            #lapply has to be used because diff() does not work on dartaframes
Mex_d_TV_diff  = data.frame(Date = Mex_d_TV$Date[-1], Mex_d_TV_diff)

colSums(apply(Mex_d_TV_diff,2, FUN = is.na))
ADF_result_l = apply(Mex_d_TV[Vars_ADF], 2, FUN = ur.df, type = "none",
                     selectlags = "AIC" )
ADF_result_d = apply(Mex_d_TV_diff[Vars_ADF], 2, FUN = ur.df, type = "none",
                     selectlags = "AIC" )

for (i in 1:length(Vars_ADF)){
  x = summary(ADF_result_l[[i]])@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
  print(paste("Levels - ", Vars_ADF[i], ". t-stat - ", round(x[1],3),
              "p-val - ", round(x[2],3) ))
  x = summary(ADF_result_d[[i]])@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
  print(paste("Diff - ", Vars_ADF[i], ". t-stat - ", round(x[1],3),
              "p-val - ", round(x[2],3) ))
} #shows that all variables have unit root and are stationary at first differences.

rm(all_days,x, Vars_ADF)

# Code no longer used -----------------------------------------------------

#Implements 
# 1. This file runs ADF test on levels and first differences on weekly data 
#     using Mex_wand the results are stored in ADF_tab for weekly data. 
# 2. The first differenced Mex_w_d created is later used in 2_SVAR.

# Running ADF Analysis on weekly  data -------------------------------------

## Creating date indices for subsetting ------------------------------------
# Index = cbind(
#   Mex_w$Date <= as.Date("2011-12-31") & Mex_w$Date >= as.Date("2010-01-01"),
#   Mex_w$Date <= as.Date("2013-12-31") & Mex_w$Date >= as.Date("2012-01-01"),
#   Mex_w$Date <= as.Date("2015-12-31") & Mex_w$Date >= as.Date("2014-01-01"))
# 
# # Vars_ADF = c("TIIE","MPTBA", "MPTBF", "GMXN01Y", "GMXN02Y", "GMXN05Y","GMXN10Y",
# #              "GMXN30Y", "MXN_USD")                                  #List of variables ADF test is to be run on
# Vars_NoADF = c("Date","MPTB1", "F_Own", "F_Own_p","Tgt_rate")
# Vars_ADF = names(Mex_w)[!names(Mex_w) %in% Vars_NoADF]
# 
# ## Creating table to store results -----------------------------------------
# 
# nvar = length(Vars_ADF)                                                           #nvar and nsubsamp are used later and so storing as a variable is useful
# nsubsamp = ncol(Index)
# ADF_tab = matrix(NaN, nvar*2, nsubsamp*2)
# colnames(ADF_tab) = paste(c(rep("Jan2010_Dec2011",2), rep("Jan2012_Dec 2013",2),
#                             rep("Jan2014_Dec2015",2)),
#                           rep(c("Lvl","1Diff"),3), sep = "_" )
# x = NaN
# for (Var in 1:nvar){
#   x = c(x,paste(rep(Vars_ADF[Var],2), c("ADF","pValue"), sep = "_") )
# }
# rownames(ADF_tab) = x[-1]
# 
# ## Creating first differences ----------------------------------------------
# 
# Mex_w_d = lapply(Mex_w[,Vars_ADF ], diff)                                          #lapply has to be used because diff() does not work on dartaframes
# Mex_w_d = data.frame(Date = Mex_w$Date[-1], Mex_w_d)
# 
# ## Running ADF test and storing results-------------------------------------
# # ADF test without drift or trend is run and this choice was made looking at 
# # the ACFs
# 
# for (subsamp in  1:nsubsamp){                                                            #Comparing speeds and reading online, for loop are not any slower (and sometimes faster) than lapply
#   for (Var in 1:nvar){
#     level_dat = Mex_w[Index[,subsamp],Vars_ADF[Var]]
#     diff_dat = Mex_w_d[Index[,subsamp],Vars_ADF[Var]]
#     
#     if (all(!is.na(level_dat)) ){
#       adf_l = summary(ur.df(level_dat, type = "none",selectlags = "AIC"))
#       ADF_tab[c(2*Var - 1,2* Var),(2*subsamp - 1)] = 
#         adf_l@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
#     }else{
#       ADF_tab[c(2*Var - 1,2* Var),(2*subsamp - 1)] = NaN
#     }
#     
#     if (all(!is.na(diff_dat)) ){
#       adf_d = summary(ur.df(diff_dat, type = "none", selectlags = "AIC"))
#       ADF_tab[c(2*Var - 1,2* Var),(2*subsamp )] =
#         adf_d@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
#     }else{
#       ADF_tab[c(2*Var - 1,2* Var),(2*subsamp )] = NaN
#     }
#   }
# }
# 
# 
# #this was used to determine the type of ADF test to be used
# #acf(adf_l$TIIE@res)
# #acf(adf_low_l$r10y_i@res)
# #acf(adf_low_l$TIIE@res)
# #acf(adf_low_l$MXN_USD_i@res)
# 
# # Removing excess variables -----------------------------------------------
# 
#  rm(x,nvar,nsubsamp, Vars_ADF, Vars_NoADF,Index, adf_l, adf_d, subsamp, Var)
