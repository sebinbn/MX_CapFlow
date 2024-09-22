# This code uses Mex_mat to create a time plot of share of long term bonds in
# portfolios of residents and non-residents. 


cols = c("Date","R_lt_share","F_lt_share","GG_lt_share")
mat_long = melt(Mex_mat[cols], id.vars = "Date")
Mat_plot = ggplot(data = mat_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  scale_color_discrete(labels = c("Residents","Non-Residents", "Total")) +
  labs(x = element_blank(), y = 'Share of long term bonds in total holdings')+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.position = c(0.1, 0.9),legend.title = element_blank(),
        legend.background = element_rect(linetype="solid",colour ="darkblue"))      

