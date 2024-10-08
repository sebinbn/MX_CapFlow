#This code creates few figures of which 1 is used in the paper:
# 1. Plotting the 1mo and 10yr yield over time from weekly data. (used in paper)
# 2. Plotting the 1mo and 10yr yield over time from daily data.
# 3. Plotting various short term yields over time from weekly data for comparison.


# Fig 1 - TIIE,1mo and 10yr from weekly data -----------------------------------

Mex_long <- melt(Mex_w[,c("Date","TIIE","MPTBA","GMXN10Y")], id.vars = "Date")

Yield_plot = ggplot(Mex_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  labs(y = "Yield on Mexican govt bonds (in %)", x = element_blank()) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_discrete(labels = c( "Overnight rate", "1 month yield", "10 year yield")) + 
  geom_rect(aes(xmin=as.Date("2010-01-01"), xmax=as.Date("2011-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color ='red', linewidth = 1.2)+
  geom_rect(aes(xmin=as.Date("2012-01-01"), xmax=as.Date("2013-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color = 'green', linewidth = 1.2)+
  geom_rect(aes(xmin=as.Date("2014-01-01"), xmax=as.Date("2015-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color = 'green', linewidth = 1.2)+
  theme_minimal()+
  theme(legend.position = c(0.7, 0.9),legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(linetype="solid",colour ="darkblue"),
        axis.text = element_text(size = 14), axis.title = element_text(size = 15))

# Fig 2 - 1mo and 10yr from weekly data -----------------------------------

Mex_long <- melt(Mex_w[,c("Date","MPTBA","GMXN10Y")], id.vars = "Date")

Yield_plot = ggplot(Mex_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  labs(y = "Yield on Mexican govt bonds (in %)", x = element_blank()) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_discrete(labels = c( "1 month yield", "10 year yield")) + 
  geom_rect(aes(xmin=as.Date("2010-01-01"), xmax=as.Date("2011-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color ='red', linewidth = 1.2)+
  geom_rect(aes(xmin=as.Date("2014-01-01"), xmax=as.Date("2015-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color = 'red', linewidth = 1.2)+
  theme_minimal()+
  theme(legend.position = c(0.7, 0.9),legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(linetype="solid",colour ="darkblue"),
        axis.text = element_text(size = 14), axis.title = element_text(size = 15))


# Fig 2 - 1mo and 10yr from daily data ------------------------------------

Mex_long <- melt(Mex_d[,c("Date","MPTBA","GMXN10Y")], id.vars = "Date")

ggplot(Mex_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  labs(y = "Yield on Mexican govt bonds (in %)", x = element_blank()) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_discrete(labels = c("1 month yield", "10 year yield")) + #scale_color_discrete(labels = c("Overnight yield", "10 year yield")) +
  geom_rect(aes(xmin=as.Date("2006-09-01"), xmax=as.Date("2008-07-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color ='red', linewidth = 1.2)+
  annotate("text", x = as.Date("2007-07-31"), y = 11, label = expression(rho - 0.081), size = 5 )+ 
  geom_rect(aes(xmin=as.Date("2014-01-01"), xmax=as.Date("2015-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color = 'red', linewidth = 1.2)+
  annotate("text", x = as.Date("2014-12-31"), y = 11, label = expression(rho - 0.169), size = 5)+ 
  theme_minimal()+
  theme(legend.position = c(0.7, 0.9),legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(linetype="solid",colour ="darkblue"),
        axis.text = element_text(size = 14), axis.title = element_text(size = 15))


# Fig 3 - comparing short term yields -------------------------------------
Date_sub = Mex_w$Date <= as.Date('2015-12-31') & Mex_w$Date >= as.Date('2010-01-01')
Mex_long <- melt(Mex_w[Date_sub,c("Date","Tgt_rate","TIIE", "MPTBA")],
                 id.vars = "Date")

ggplot(Mex_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  labs(y = "Interest rates (in %)", x = element_blank()) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_discrete(labels = c("Policy rate", "TIIE", "1 month yield")) + #scale_color_discrete(labels = c("Overnight yield", "10 year yield")) +
  geom_vline(xintercept = as.Date("2011-12-31"), color = 'black', 
             linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = as.Date("2014-01-01"), color = 'black', 
             linetype = "dashed", linewidth = 1.2) +
  theme_minimal()+
  theme(legend.position = c(0.8, 0.9),legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(linetype="solid",colour ="darkblue"),
        axis.text = element_text(size = 14), axis.title = element_text(size = 15))

# Fig 4 - Depicting slope -------------------------------------

Mex_w$`10y-TIIE` = Mex_w$GMXN10Y - Mex_w$TIIE
Mex_w$`10y-Tgt` = Mex_w$GMXN10Y - Mex_w$Tgt_rate
Mex_w$`10y-1mo` = Mex_w$GMXN10Y - Mex_w$MPTBA
Mex_long <- melt(Mex_w[,c("Date" ,"10y-Tgt","10y-TIIE", "10y-1mo")],id.vars = "Date")

ggplot(Mex_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  labs(y = "Slopes", x = element_blank()) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_discrete(labels = c("10yr - Policy rate", "10yr - TIIE", "10yr - 1 mo")) + #scale_color_discrete(labels = c("Overnight yield", "10 year yield")) +
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed", linewidth = 1.2) +
  theme_minimal()+
  theme(legend.position = c(0.8, 0.9),legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(linetype="solid",colour ="darkblue"),
        axis.text = element_text(size = 14), axis.title = element_text(size = 15))
