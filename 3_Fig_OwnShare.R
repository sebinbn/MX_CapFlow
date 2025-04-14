# This code uses Own_Data to create following figures.
# 1. A time plot showing varying shares of ownership by classes over time.
# 2. A time plot showing varying value of ownership by classes over time.


#Earlier pics not used anymore.
# 1. A time plot showing varying value of ownership by classes over time with 
#    analysis period marked with rectangles 
# 2. A time plot showing varying value of ownership by classes over time for all
#    data available (1970 onwards)
# 3. Two time plots showing ownership proportion in the two subsamples 
#    2010 Jan - 2011 Dec, 2014 Jan - 2015 Dec 
# 4. Barchart showing change in ownership proportion in the two subsamples 
#    2010 Jan - 2011 Dec, 2014 Jan - 2015 Dec 


# Plot 1: Share over time ---------------------------------------------------

cols = c("Date","SF65216_p","Banxico_p","SF65211_p","SF65214_p","SF65215_p",
         "SF65213_p","SF65218_p")

own_long = melt(Own_Data[Own_Data$Date <= as.Date("2023-12-31") & 
                           Own_Data$Date >= as.Date("2006-01-01"),cols],
                id.vars = "Date")
Share_plot = ggplot(data = own_long, aes(x = Date, y = value, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_brewer(labels = c("Others","Banxico","Banks","Invst. funds",
                               "Insurance Co.","Pension funds","Non-residents"),
                    palette = "Set3") +
  guides(color = "none", fill = guide_legend(reverse = TRUE)) +
  scale_x_date(date_labels = '%Y', date_breaks = "2 year", expand = c(0, 0))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = 'Ownership share in govt bonds O/S', x = element_blank())+ 
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        legend.position = "bottom")

Share_plot

# Plot 2 : Value over time ---------------------------------------------------

cols = c("Date","SF65216","Banxico","SF65211","SF65214","SF65215",
         "SF65213","SF65218")

own_long = melt(Own_Data[Own_Data$Date >= as.Date("2006-01-01") & 
                           Own_Data$Date <= as.Date("2023-12-31"),
                           cols],
                id.vars = "Date")
Value_plot = ggplot(data = own_long, aes(x = Date, y = value/1000000, color = variable)) +
  geom_area(aes(fill = variable)) +
  scale_fill_brewer(labels = c("Others","Banxico","Banks","Invst. funds",
                               "Insurance Co.","Pension funds","Non-residents"),
                    palette = "Set3") +
  guides(color = "none", fill = guide_legend(reverse = TRUE)) +
  scale_x_date(date_labels = '%Y', date_breaks = "2 year", expand = c(0, 0))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = 'Trillions of Pesos', x = element_blank())+ 
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        legend.title = element_blank(),legend.text = element_text(size = 14),
        legend.position = "bottom")
Value_plot

# Removing excess variables -----------------------------------------------

rm(own_long)# own_long_all,own_long1,own_long2)


# Unused plots ------------------------------------------------------------

# Plot 1 unused: Value over time ---------------------------------------------------

# OwnShare_plot1 = ggplot(data = own_long, aes(x = Date, y = value, color = variable)) +
#   geom_area(aes(fill = variable)) +
#   scale_fill_brewer(labels = c("Others","Banxico","Banks","Invst. funds",
#                                "Insurance Co.","Pension funds","Non-residents"),
#                     palette = "Set3") +
#   guides(color = "none", fill = guide_legend(reverse = TRUE)) +
#   geom_rect(aes(xmin=as.Date("2010-01-01"), xmax=as.Date("2011-12-21"),
#                 ymin=-Inf,ymax=Inf),
#             fill = NA, alpha= 0.01, color ='red', linewidth = 1.2)+
#   geom_rect(aes(xmin=as.Date("2012-01-11"), xmax=as.Date("2013-12-31"),
#                 ymin=-Inf,ymax=Inf),
#             fill = NA, alpha= 0.01, color = 'darkgreen', linewidth = 1.2)+
#   labs(y = 'Ownership share in govt bonds O/S', x = element_blank())+ 
#   scale_x_date(date_labels = '%Y', date_breaks = "2 year")+
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
#         legend.title = element_blank(),legend.text = element_text(size = 14),
#         legend.position = "bottom")


# Plot 2: Value over time (all data 1970 onwards) ----------------------------------------

# own_long_all = melt(Own_Data[,cols],
#                     id.vars = "Date")
# ggplot(data = own_long_all, aes(x = Date, y = value, color = variable)) +
#   geom_area(aes(fill = variable)) +
#   scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
#                                  "Pension funds","Non-residents") ) +
#   guides(color = "none") +
#   labs(y = 'Ownership share in govt bonds O/S', x = element_blank())+ 
#   scale_x_date(date_labels = '%Y', date_breaks = "2 year")+
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
#         legend.title = element_blank(),legend.text = element_text(size = 14),
#         legend.position = "bottom")


# Plot 3: Share in subsamples -----------------------------------------------

# own_long1 = melt(Own_Data[Own_Data$Date >= as.Date("2010-01-01") & 
#                            Own_Data$Date <= as.Date("2011-12-31"), 
#                          cols], id.vars = "Date")
# own_long2 = melt(Own_Data[Own_Data$Date >= as.Date("2014-01-01") & 
#                            Own_Data$Date <= as.Date("2015-12-31"), 
#                          cols], id.vars = "Date")
# 
# 
# own_plot1 = ggplot(data = own_long1, aes(x = Date, y = value, color = variable)) +
#   geom_area(aes(fill = variable)) +
#   scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
#                                  "Pension funds","Non-residents") ) +
#   guides(color = "none") +
#   labs(x = element_blank(), y = 'Share of bonds O/S', 
#        title = "2010 Jan - 2011 Dec")+
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
#         axis.text.x = element_blank(),legend.title = element_blank(),
#         legend.position = "none", legend.text = element_text(size = 14))
# 
# 
# own_plot2 = ggplot(data = own_long2, aes(x = Date, y = value, color = variable)) +
#   geom_area(aes(fill = variable)) +
#   scale_fill_discrete(labels = c("Others","Banxico","Banks","Invst. funds", "Insurance Co.",
#                                  "Pension funds","Non-residents")) +
#   guides(color = "none") +
#   labs(x = element_blank(), y = element_blank(),title = "2014 Jan - 2015 Dec")+
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
#         axis.text.x = element_blank(),legend.title = element_blank(),
#         legend.position = "none", legend.text = element_text(size = 14))
# 
# OwnShareComp_plot = ggarrange(own_plot1, own_plot2, common.legend = T, legend = "bottom")

# Plot 4: Share in subsamples -----------------------------------------------

# This code uses data in Own_Data for ownership by different categories. It
# then calculates the difference in average proportion between both analysis
# periods and creates the same in a barchart.

# Owners = paste(c("Banxico","SF65211", "SF65213","SF65214", "SF65215","SF65216",
#                  "SF65218"), "_p", sep = "")  
# change = colMeans(Own_Data[Own_Data$Date >= as.Date("2014-01-01") & 
#                              Own_Data$Date <= as.Date("2015-12-31"),Owners]) - 
#   colMeans(Own_Data[Own_Data$Date >= as.Date("2010-01-01") & 
#                       Own_Data$Date <= as.Date("2011-12-31"),Owners])
# 
# Owner_names = c("Banxico","Banks","Pension funds","Invst. funds", 
#                 "Insurance & Surety Co.","Others","Non-residents")
# barData = data.frame(Owner = Owner_names, Change = change )
# 
# #Creating barchart of change in proportion
# barData$Owner = with(barData, reorder(Owner, Change))
# barData$increase = barData$Change >=0
# barchart = ggplot(data = barData, aes(x = Owner, y = Change*100, fill = increase))+
#   geom_bar(stat="identity")+
#   scale_fill_manual(values = c("firebrick2","seagreen3")) +
#   coord_flip()+
#   labs(x = element_blank(), y = "Change in proportion shares owned b/w both SVAR periods (in pp)")+
#   theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
#         legend.position = "none")
# 
# #trying to identify the maximum value of Other ownership
# Own_Data[which.max(Own_Data$SF65216_p),c('Date',Owners)]
