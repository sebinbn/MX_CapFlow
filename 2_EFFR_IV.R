# This file runs IV regressions

library(ivreg)
# Checking Relevance ------------------------------------------------------

EFFR_w$F_Own_p = Mex_w$F_Own_p
summary(lm(F_Own_p ~ EFFR, data = EFFR_w))

##Plotting EFFR and Capital inflows
# EFFR_long = melt(EFFR_w, id.vars = "Date")
# ggplot( data = EFFR_long, aes(x = Date, y = value, color = variable)) +
#   geom_line(linewidth = 1.25)


# 2SLS --------------------------------------------------------------------
EFFR_w[c('BA1mo', 'BA10Y')] = Liq_w[c('BA_TBA', 'BA_10Y')]
Stage1 = lm(F_Own_p*100 ~ EFFR, data = EFFR_w)
Stage2 = lm(EFFR_w$BA1mo ~ Stage1$fitted.values)
summary(Stage2)
EFFR_w$F_Own_pcnt = EFFR_w$F_Own_p * 100
EFFR_IVreg = ivreg(BA1mo ~ F_Own_pcnt | EFFR, data = EFFR_w)
summary(EFFR_IVreg$coefficients1)
coef(EFFR_IVreg, component = c("stage2", "stage1"), complete = TRUE)
