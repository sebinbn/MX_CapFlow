# The results of rolling horizon SVAR are regressed on foreign ownership. Uses
# MergedDat and #MergedCumDat created by 3_IRF_MH.R

#The units of Mex_Fo is the actual value.

library(forecast)
# Regressions on contemporaneous IRF --------------------------------------

reg1mo30y = lm(IRF_1mo30y~FO, data = MergedDat)
summary(reg1mo30y)

regON1mo = lm(IRF_ON1mo~FO, data = MergedDat)
summary(regON1mo)

regON30y = lm(IRF_ON30y~FO, data = MergedDat)
summary(regON30y)

# Regressions on cumulative IRF ----------------------------------------

regON1moCum = lm(IRF_ON1mo~FO_prop, data = MergedCumDat)
summary(regON1moCum)

regON10yCum = lm(IRF_ON10y~FO_prop, data = MergedCumDat)
summary(regON10yCum)

regON30yCum = lm(IRF_ON30y~FO_prop, data = MergedCumDat)
summary(regON30yCum)

reg1mo10yCum = lm(IRF_1mo10y~FO_prop, data = MergedCumDat)
summary(reg1mo10yCum)

reg1mo30yCum = lm(IRF_1mo30y~FO_prop, data = MergedCumDat)
summary(reg1mo30yCum)


# Regressions with AR1 errors

print("The average cum.IRF values and FO are")
print(apply(MergedCumDat[,-1], 2, FUN = mean)) #finding means to write in paper


ARON1moCum = Arima(MergedCumDat$IRF_ON1mo, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
(1-pnorm(abs(ARON1moCum$coef)/sqrt(diag(ARON1moCum$var.coef))))*2
ARON1moCum
ARON10yCum = Arima(MergedCumDat$IRF_ON10y, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
(1-pnorm(abs(ARON10yCum$coef)/sqrt(diag(ARON10yCum$var.coef))))*2
ARON10yCum
ARON30yCum = Arima(MergedCumDat$IRF_ON30y, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
(1-pnorm(abs(ARON30yCum$coef)/sqrt(diag(ARON30yCum$var.coef))))*2
ARON30yCum
AR1mo10yCum = Arima(MergedCumDat$IRF_1mo10y, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
(1-pnorm(abs(AR1mo10yCum$coef)/sqrt(diag(AR1mo10yCum$var.coef))))*2
AR1mo10yCum
AR1mo30yCum = Arima(MergedCumDat$IRF_1mo30y, order = c(1,0,0), xreg = cbind(MergedCumDat$FO_prop))
(1-pnorm(abs(AR1mo30yCum$coef)/sqrt(diag(AR1mo30yCum$var.coef))))*2
AR1mo30yCum

# Cochrane-Orcutt Procedure ----------------------------------------
plot(reg1mo30yCum$residuals)
pacf(reg1mo30yCum$residuals) # shows significant 1st lag
CO_AR_1mo30y = arima(reg1mo30yCum$residuals, order = c(1,0,0), include.mean = F)
CO_AR_1mo30y$coef

plot(regON1moCum$residuals)
pacf(regON1moCum$residuals) # also shows significant 1st lag
CO_AR_ON1mo = arima(regON1moCum$residuals, order = c(1,0,0), include.mean = F)
CO_Proc_AR_ON1mo$coef


# Other (previous code) ----------------------------------------
# reg1mo10yCum = lm(IRF_1mo10y~FO, data = MergedCumDat)
# summary(reg1mo10yCum)
# 
# regON1moCum1 = lm(IRF_ON1mo~FO, data = MergedCumDat)
# summary(regON1moCum1)

# some summary stats to put the outcome in perspective

colMeans(Mex_FO[Mex_FO$Date <= as.Date("2019-02-25") & Mex_FO$Date >= as.Date("2018-02-25"),
           "F_Own"])
colMeans(Mex_FO[Mex_FO$Date <= as.Date("2023-12-25") & Mex_FO$Date >= as.Date("2023-01-25"),
                "F_Own"])
colMeans(Mex_FO[Mex_FO$Date <= as.Date("2010-12-25") & Mex_FO$Date >= as.Date("2010-01-25"),
                "F_Own"])
colMeans(Mex_FO[Mex_FO$Date <= as.Date("2014-12-25") & Mex_FO$Date >= as.Date("2014-01-25"),
                "F_Own"])
