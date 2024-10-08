# This file runs ADF test on levels and first differences and the results are
# stored in ADF_tab. The ADF test is run on weekly data using Mex_w. The first
# differenced Mex_w is later used in 2_SVAR

library(urca)         #for ur.df

# Creating date indices for subsetting ------------------------------------
Index = cbind(
  Mex_w$Date <= as.Date("2011-12-31") & Mex_w$Date >= as.Date("2010-01-01"),
  Mex_w$Date <= as.Date("2013-12-31") & Mex_w$Date >= as.Date("2012-01-01"),
  Mex_w$Date <= as.Date("2015-12-31") & Mex_w$Date >= as.Date("2014-01-01"))

Vars_ADF = c("TIIE","MPTBA", "MPTBF", "GMXN01Y", "GMXN02Y", "GMXN05Y","GMXN10Y",
             "GMXN30Y", "MXN_USD")                                  #List of variables ADF test is to be run on


# Creating table to store results -----------------------------------------

nvar = length(Vars_ADF)                                                           #nvar and nsubsamp are used later and so storing as a variable is useful
nsubsamp = ncol(Index)
ADF_tab = matrix(NaN, nvar*2, nsubsamp*2)
colnames(ADF_tab) = paste(c(rep("Jan2010_Dec2011",2), rep("Jan2012_Dec 2013",2),
                            rep("Jan2014_Dec2015",2)),
                          rep(c("Lvl","1Diff"),3), sep = "_" )
x = NaN
for (Var in 1:nvar){
  x = c(x,paste(rep(Vars_ADF [Var],2), c("ADF","pValue"), sep = "_") )
}
rownames(ADF_tab) = x[-1]

# Creating first differences ----------------------------------------------

Mex_w_d = lapply(Mex_w[,Vars_ADF ], diff)                                          #lapply has to be used because diff() does not work on dartaframes
Mex_w_d = data.frame(Date = Mex_w$Date[-1], Mex_w_d)

# Running ADF test and storing results-------------------------------------
# ADF test without drift or trend is run and this choice was made looking at 
# the ACFs

for (subsamp in  1:nsubsamp){                                                            #Comparing speeds and reading online, for loop are not any slower (and sometimes faster) than lapply
  for (Var in 1:nvar){
    level_dat = Mex_w[Index[,subsamp],Vars_ADF [Var]]
    diff_dat = Mex_w_d[Index[,subsamp],Vars_ADF [Var]]
    
    if (all(!is.na(level_dat)) ){
      adf_l = summary(ur.df(level_dat, type = "none",selectlags = "AIC"))
      ADF_tab[c(2*Var - 1,2* Var),(2*subsamp - 1)] = 
        adf_l@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
    }else{
      ADF_tab[c(2*Var - 1,2* Var),(2*subsamp - 1)] = NaN
    }
    
    if (all(!is.na(diff_dat)) ){
      adf_d = summary(ur.df(diff_dat, type = "none", selectlags = "AIC"))
      ADF_tab[c(2*Var - 1,2* Var),(2*subsamp )] =
        adf_d@testreg$coefficients[1,c("t value", "Pr(>|t|)")]
    }else{
      ADF_tab[c(2*Var - 1,2* Var),(2*subsamp )] = NaN
    }
  }
}


#this was used to determine the type of ADF test to be used
#acf(adf_l$TIIE@res)
#acf(adf_low_l$r10y_i@res)
#acf(adf_low_l$TIIE@res)
#acf(adf_low_l$MXN_USD_i@res)


# Removing excess variables -----------------------------------------------

 rm(x,nvar,nsubsamp, Vars_ADF,Index, adf_l, adf_d, subsamp, Var)



