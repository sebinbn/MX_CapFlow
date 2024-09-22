# This code uses Mex_FO to create two figures:
# 1. Figure showing value of foreign owned government bonds over time
# 2. Share of foreign owned government bonds over time. 


# Fig 1 - Value -----------------------------------------------------------

FO_value_plot = ggplot(Mex_FO[Mex_FO$Date >= as.Date("1994-01-01") & Mex_FO$Date <= as.Date("2023-12-31"),],
       aes(x = Date, y = F_Own/1000)) +
  geom_line(linewidth = 1.2, color = "blue") +
  #geom_vline(xintercept = as.numeric(as.Date("2012-01-04")),linewidth = 1.4, 
  #          linetype = "dashed", color = "red") +
  #geom_text(aes(x = as.Date("2011-12-25"), y = 1750, label = "Jan 2012 (Break 2)"), 
  #         vjust = -0.5, color = "red", size = 4.5, angle = 90) +
  labs(x = element_blank(), y = 'Foreign ownership of bonds (Billions of Pesos )')+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels = '%Y', date_breaks = "4 year")

# Fig 2 - Proportion -------------------------------------------------------

FO_share_plot = ggplot(Mex_FO[Mex_FO$Date >= as.Date("1994-01-01") & Mex_FO$Date <= as.Date("2023-12-31"),], 
       aes(x = Date, y = F_Own_p)) +
  geom_line(linewidth = 1.2, color = "blue") +
  geom_rect(aes(xmin=as.Date("2010-01-01"), xmax=as.Date("2011-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color ='red', linewidth = 1.2)+
  geom_rect(aes(xmin=as.Date("2014-01-01"), xmax=as.Date("2015-12-31"),ymin=-Inf,ymax=Inf),
            fill = NA, alpha= 0.01, color = 'red', linewidth = 1.2)+
   labs(x = element_blank(), y = 'Non-resident owned share in bonds O/S (in %)')+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels = '%Y', date_breaks = "4 year")




