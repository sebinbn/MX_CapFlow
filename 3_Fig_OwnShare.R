# This code uses Own_Data to create 3 figures.
# 1. A time plot showing varying shares of ownership over time used in paper.
# 2. Above time plot with all available data. (not used in paper)
# 3. Two time plots showing ownership proportion in the two subsamples (not used in paper)


# Plot: Share over time ---------------------------------------------------

cols = c("Date","SF65216_p","Banxico_p","SF65211_p","SF65214_p","SF65215_p",
         "SF65213_p","SF65218_p")

own_long = melt(Own_Data[Own_Data$Date <= as.Date("2023-12-31") & 
                           Own_Data$Date >= as.Date("2006-01-01"),cols],
                id.vars = "Date")
OwnShare_plot = ggplot(data = own_long, aes(x = Date, y = value, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
                                 "Pension funds","Non-residents") ) +
  guides(color = "none") +
  labs(y = 'Ownership share in govt bonds O/S', x = element_blank())+ 
  scale_x_date(date_labels = '%Y', date_breaks = "2 year")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        legend.position = "bottom")


# Plot: Share over time (all data) ----------------------------------------

own_long_all = melt(Own_Data[,cols],
                    id.vars = "Date")
ggplot(data = own_long_all, aes(x = Date, y = value, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
                                 "Pension funds","Non-residents") ) +
  guides(color = "none") +
  labs(y = 'Ownership share in govt bonds O/S', x = element_blank())+ 
  scale_x_date(date_labels = '%Y', date_breaks = "2 year")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        legend.position = "bottom")


# Plot: Share in subsamples -----------------------------------------------

own_long1 = melt(Own_Data[Own_Data$Date >= as.Date("2010-01-01") & 
                           Own_Data$Date <= as.Date("2011-12-31"), 
                         cols], id.vars = "Date")
own_long2 = melt(Own_Data[Own_Data$Date >= as.Date("2014-01-01") & 
                           Own_Data$Date <= as.Date("2015-12-31"), 
                         cols], id.vars = "Date")


own_plot1 = ggplot(data = own_long1, aes(x = Date, y = value, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
                                 "Pension funds","Non-residents") ) +
  guides(color = "none") +
  labs(x = element_blank(), y = 'Share of bonds O/S', 
       title = "2010 Jan - 2011 Dec")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        axis.text.x = element_blank(),legend.title = element_blank(),
        legend.position = "none", legend.text = element_text(size = 14))


own_plot2 = ggplot(data = own_long2, aes(x = Date, y = value, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
                                 "Pension funds","Non-residents")) +
  guides(color = "none") +
  labs(x = element_blank(), y = element_blank(),title = "2014 Jan - 2015 Dec")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        axis.text.x = element_blank(),legend.title = element_blank(),
        legend.position = "none", legend.text = element_text(size = 14))

OwnShareComp_plot = ggarrange(own_plot1, own_plot2, common.legend = T, legend = "bottom")


# Removing excess variables -----------------------------------------------

rm(own_long, own_long_all,own_long1,own_long2)
