# This code uses data in Own_Data for ownership by different categories. It
# then calculates the difference in average proportion between both analysis
# periods and creates the same in a barchart.

Owners = paste(c("Banxico","SF65211", "SF65213","SF65214", "SF65215","SF65216",
                 "SF65218"), "_p", sep = "")  
change = colMeans(Own_Data[Own_Data$Date >= as.Date("2014-01-01") & 
                             Own_Data$Date <= as.Date("2015-12-31"),Owners]) - 
  colMeans(Own_Data[Own_Data$Date >= as.Date("2010-01-01") & 
                      Own_Data$Date <= as.Date("2011-12-31"),Owners])

Owner_names = c("Banxico","Banks","Pension funds","Invst. funds", 
                "Insurance & Surety Co.","Others","Non-residents")
barData = data.frame(Owner = Owner_names, Change = change )

#Creating barchart of change in proportion
barData$Owner = with(barData, reorder(Owner, Change))
barData$increase = barData$Change >=0
barchart = ggplot(data = barData, aes(x = Owner, y = Change*100, fill = increase))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("firebrick2","seagreen3")) +
  coord_flip()+
  labs(x = element_blank(), y = "Change in proportion shares owned b/w both SVAR periods (in pp)")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.position = "none")

#trying to identify the maximum value of Other ownership
Own_Data[which.max(Own_Data$SF65216_p),c('Date',Owners)]
