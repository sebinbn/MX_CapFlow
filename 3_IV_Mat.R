# This code uses Y_FO_results from 2_IVRegs_Mat.R


Y_FO_Dat = Y_FO_results[-16,] #removing 1 week data
Y_FO_Dat$Var = paste(substr(Y_FO_Dat$Var,5,6),"Yr")
Y_FO_Dat$Var[14:17] = c("1 Mo","3 Mo","9 Mo","6 Mo")
Y_FO_Dat = Y_FO_Dat[c(14,15,17,16, 1:13),] #rearranging by maturity
Y_FO_Dat$Var = factor(Y_FO_Dat$Var,levels = Y_FO_Dat$Var) 

ggplot(Y_FO_Dat, aes(x =Var)) +
  geom_point(aes(y = coef), size = 2) +
  geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, color = "blue") +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) + 
  scale_x_discrete(breaks = Y_FO_Dat$Var[seq(1, length(Y_FO_Dat$Var), by = 2)])+
  labs(x = 'Maturity', y = 'Basis points', title = 'Impact on yields from 1pp increase in FO') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16 ),
        legend.title = element_blank(), legend.position = c(0.2, 0.9))


# Plotting impact on spread -----------------------------------------------

S_FO_Dat = S_FO_results[-c(16),]
S_FO_Dat$Var = paste(substr(S_FO_Dat$Var,4,5),"Yr")
S_FO_Dat$Var[14:17] = c("1 Mo","3 Mo","9 Mo","6 Mo")
S_FO_Dat = S_FO_Dat[c(14,15,17,16, 1:13),] #rearranging by maturity
S_FO_Dat$Var = factor(S_FO_Dat$Var,levels = S_FO_Dat$Var) 

ggplot(S_FO_Dat, aes(x =Var)) +
  geom_point(aes(y = coef), size = 2) +
  geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, color = "blue") +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) + 
  scale_x_discrete(breaks = Y_FO_Dat$Var[seq(1, length(Y_FO_Dat$Var), by = 2)])+
  labs(x = 'Maturity', y = 'Basis points', title = 'Impact on spreads from 1pp increase in FO') +
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16 ),
        legend.title = element_blank(), legend.position = c(0.2, 0.9))
