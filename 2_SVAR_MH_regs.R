# The results of rolling horizon SVAR are regressed on foreign ownership. Uses
# MergedDat and #MergedCumDat created by 3_IRF_MH.R

#The units of Mex_Fo is share *10, this is adjusted in 3_IRF_MH.R

# Regressions on contemporaneous IRF --------------------------------------

reg1mo30y = lm(IRF_1mo30y~FO, data = MergedDat)
summary(reg1mo30y)

regON1mo = lm(IRF_ON1mo~FO, data = MergedDat)
summary(regON1mo)

regON30y = lm(IRF_ON30y~FO, data = MergedDat)
summary(regON30y)

# Regressions on cumulative IRF ----------------------------------------

reg1mo30yCum = lm(IRF_1mo30y~FO, data = MergedCumDat)
summary(reg1mo30yCum)

regON1moCum = lm(IRF_ON1mo~FO, data = MergedCumDat)
summary(regON1moCum)

regON30yCum = lm(IRF_ON30y~FO, data = MergedCumDat)
summary(regON30yCum)

# reg1mo10yCum = lm(IRF_1mo10y~FO, data = MergedCumDat)
# summary(reg1mo10yCum)
# 
# regON1moCum1 = lm(IRF_ON1mo~FO, data = MergedCumDat)
# summary(regON1moCum1)


