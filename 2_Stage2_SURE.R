# This file uses IVData_m_stat created by 2_IV_Stage1.R 
library(systemfit)
library(lmtest)

# Creating Stage 2 Data ------------------------------------------------------

Stage2_data = data.frame(
  IVData_m_stat[, c(1,19,20,23,22,6:18)],
  Stage1_fitted = Stage1$fitted.values,
  TIIE = IVData_m_stat$TIIE
)

# SURE Estimation ---------------------------------------------------------

yield_names = colnames(IVData_m_stat[, c(19,20,23,22,6:18)])
# build each equation formula
eq_list = lapply(yield_names, function(y) {
  as.formula( paste0( y, " ~ 0 + Stage1_fitted + TIIE") )
})
names(eq_list) = yield_names


fit_fgls = systemfit(eq_list, data = Stage2_data, method = "SUR")
summary(fit_fgls)


# Calculating tFse and Storing results for plotting  ---------------------------

Y_FO_results = data.frame(Var = yield_names, coef = matrix(NaN,17,1),
                          se = matrix(NaN,17,1), tFse = matrix(NaN,17,1),
                          pValue = matrix(NaN,17,1) )

pickCols = seq(1,length(fit_fgls$coefficients), by = 2)
# Results of coefficients are multipied by 100 to convert to bps
Y_FO_results$coef = fit_fgls$coefficients[pickCols]*100
Y_FO_results$se = sqrt(diag(fit_fgls$coefCov))[pickCols]*100
Y_FO_results$tFse = Y_FO_results$se*tFCorr
Y_FO_results$pValue = (1-pnorm(abs(Y_FO_results$coef)/Y_FO_results$tFse))*2


# Conducting Wald Test ----------------------------------------------------

hypotheses(fit_fgls, hypothesis = "MPTBA_Stage1_fitted - GMXN30Y_Stage1_fitted = 0") 
hypotheses(fit_fgls, hypothesis = "MPTBC_Stage1_fitted - GMXN30Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "GMXN01Y_Stage1_fitted - GMXN30Y_Stage1_fitted = 0") 
hypotheses(fit_fgls, hypothesis = "MPTBA_Stage1_fitted - GMXN20Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "MPTBC_Stage1_fitted - GMXN20Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "GMXN01Y_Stage1_fitted - GMXN20Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "MPTBA_Stage1_fitted - GMXN10Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "MPTBC_Stage1_fitted - GMXN10Y_Stage1_fitted = 0")
hypotheses(fit_fgls, hypothesis = "GMXN01Y_Stage1_fitted - GMXN10Y_Stage1_fitted = 0")


