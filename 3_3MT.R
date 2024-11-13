# This code created plots used in the 3MT slides

LowFO_mean = colMeans(BBYield[BBYield$Date >= as.Date("2006-06-01") & 
                   BBYield$Date <= as.Date("2011-12-31"),c(15,16,19,18,2:14)],
                   na.rm = T)

HighFO_mean = colMeans(BBYield[BBYield$Date >= as.Date("2013-01-01") & 
                   BBYield$Date <= as.Date("2018-12-31"),c(15,16,19,18,2:14)],
                   na.rm = T)
Maturity = paste(substr(names(LowFO_mean),5,6),"Yr")
Maturity[1:4] = c("1 Mo","3 Mo","6 Mo","9 Mo")

df_means <- data.frame(Maturity = Maturity, # column names (x-axis)
                        LowFO = LowFO_mean,            # column means for period 1
                         HighFO = HighFO_mean)            # column means for period 2
df_means$Maturity = factor(df_means$Maturity,levels = df_means$Maturity) 

# Melt the data to make it long-form (required for ggplot)
df_long <- melt(df_means, id.vars = 'Maturity', variable.name = 'Period',
                value.name = 'Mean')

# Create the ggplot with two lines
ggplot(df_long, aes(x = Maturity, y = Mean, color = Period, group = Period)) +
  geom_line(size = 1.2) +
  scale_color_discrete(labels = c("LowFO" = "Low Foreign Ownership", 
                                  "HighFO" = "High Foreign Ownership")) +
  scale_x_discrete(breaks = df_long$Maturity[seq(1, length(df_long$Maturity)/2, by = 2)])+
  labs(x = 'Maturity', y = 'Avg. Yield (in %)', title = 'Yields in High/Low Foreign ownership period') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16 ),
        legend.title = element_blank(), legend.position = c(0.2, 0.9))
