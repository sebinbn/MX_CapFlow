# This code created plots used in the 3MT slides

LowFO_mean = colMeans(BBYield[BBYield$Date >= as.Date("2006-06-01") & 
                   BBYield$Date <= as.Date("2011-12-31"),c(15,16,19,18,2:14)],
                   na.rm = T)

HighFO_mean = colMeans(BBYield[BBYield$Date >= as.Date("2013-01-01") & 
                   BBYield$Date <= as.Date("2018-12-31"),c(15,16,19,18,2:14)],
                   na.rm = T)
name = paste("Y_",substr(names(LowFO_mean),5,6))
name[1:4] = c("M_01","M_03","M_06","M_09")

df_means <- data.frame(column_names = name, # column names (x-axis)
                        LowFO = LowFO_mean,            # column means for period 1
                         HighFO = HighFO_mean)            # column means for period 2

# Melt the data to make it long-form (required for ggplot)
df_long <- melt(df_means, id.vars = 'column_names', variable.name = 'Period',
                value.name = 'Mean')

# Create the ggplot with two lines
ggplot(df_long, aes(x = column_names, y = Mean, color = Period, group = Period)) +
  geom_line(size = 1.2) +
  labs(x = 'Maturity', y = 'Avg. Yield', title = 'Lower Yields with Larger foreigners') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(), legend.position = c(0.1, 0.9))
