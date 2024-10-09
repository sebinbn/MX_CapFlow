# This file explores EFFR data downloaded from FRED and checks its suitability as
# an IV for capital inflows into bonds in Mexico

EFFR = read.csv('EFFR.csv')
EFFR$DATE = as.Date(EFFR$DATE)
EFFR$EFFR = as.numeric(EFFR$EFFR)


# Creating Weekly data ----------------------------------------------------

Sun  = seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "7 days")            #creating vector of Sundays. 2006-01-01 was Sunday.

EFFR_w = data.frame(Date = Sun,
                   Values = matrix(NaN, length(Sun),1 ) )                #Initiating a dataframe
colnames(EFFR_w)[-1] = 'EFFR'

for(i in 1:length(Sun)){                                                          
  Week_Data = EFFR[EFFR$DATE <= Sun[i] & EFFR$DATE > (Sun[i]-7), ]
  a = na.omit(Week_Data[,2])
  EFFR_w[i,2] = if(length(a) != 0) tail(a,1) else NA
} 


# Checking Relevance ------------------------------------------------------

EFFR_w$F_Own_p = Mex_w$F_Own_p
summary(lm(F_Own_p ~ EFFR, data = EFFR_w))

EFFR_long = melt(EFFR_w, id.vars = "Date")
ggplot( data = EFFR_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25)
