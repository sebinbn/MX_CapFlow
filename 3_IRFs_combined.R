# this code uses the list 'IRFs' created by 2_SVAR and plots an image with the
# first period impact at various maturities. IRFs for various
# period and variable specifications

# Function to create IRF -------------------


irfCreate = function(samp_num){
    response = c("MPTBF", "GMXN01Y","GMXN02Y", "GMXN05Y", "GMXN10Y", "GMXN30Y")
    if(samp_num <= 18){
      Resp = response[ceiling(samp_num/3)]
      # irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$MPTBA) - 1),
      #                     IRF = IRFs[[samp_num]]$irf$MPTBA[,Resp],
      #                     Upper_CI = IRFs[[samp_num]]$Upper$MPTBA[,Resp],
      #                     Lower_CI = IRFs[[samp_num]]$Lower$MPTBA[,Resp])
      irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$TIIE) - 1),
                          IRF = IRFs[[samp_num]]$irf$TIIE[,Resp],
                          Upper_CI = IRFs[[samp_num]]$Upper$TIIE[,Resp],
                          Lower_CI = IRFs[[samp_num]]$Lower$TIIE[,Resp])
    }else if(samp_num >= 22){
      irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$TIIE) - 1),
                          IRF = IRFs[[samp_num]]$irf$TIIE[,'MPTBA'],
                          Upper_CI = IRFs[[samp_num]]$Upper$TIIE[,'MPTBA'],
                          Lower_CI = IRFs[[samp_num]]$Lower$TIIE[,'MPTBA'])
    }else{
      irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$TIIE) - 1),
                          IRF = IRFs[[samp_num]]$irf$TIIE[,'GMXN10Y'],
                          Upper_CI = IRFs[[samp_num]]$Upper$TIIE[,'GMXN10Y'],
                          Lower_CI = IRFs[[samp_num]]$Lower$TIIE[,'GMXN10Y'])
    }
    return(irfPlot(irfDat,samp_num))
}

# Function to plot IRF using ggplot ---------------------------------------

irfPlot = function(Data, samp_num){
  # Identify response variable name from sample number
  response = c("6mo", "01y","02y", "05y", "10y", "30y", "10y","1mo")
  Response = c("6 mo", "1 yr","2 yr", "5 yr", "10 yr", "30 yr", "10 yr","1 mo")
  Resp = Response[ceiling(samp_num/3)]
  resp = response[ceiling(samp_num/3)]
  
  # Identify impulse variable name from sample number
  # if(samp_num <= 18){
  #   implse = "1mo"
  #   Implse = "1 mo"
  # }else{
  #   implse = "ON"
  #   Implse = "Overnight"
  # }
  implse = "ON"
  Implse = "Overnight"
  
  # Identify period from sample number
  if (samp_num %% 3 == 1){
    period = '2010-11'
  }else if (samp_num %% 3 == 2){
    period = '2012-13'
  }else if (samp_num %% 3 == 0){
    period = '2014-15'
  }
  
  IRFPlot = ggplot(Data, aes(x = Week)) +
    geom_line(aes(y = IRF), color = "black", linewidth = 1.2) +                     # Main IRF line
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = 'blue', alpha = 0.3) +  # CI band
    geom_hline(yintercept = 0, color = 'black', linetype = "dashed", linewidth = 1) +  # Zero line
    scale_y_continuous(limits = c(-0.4, 0.85), breaks = seq(-0.2, 0.8, by = 0.1)) +
    labs(title = paste("Impulse Response to", Implse, "yield"), x = "Week",
         y = paste('\u0394', Resp, "yield") ) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
  path = substr(getwd(), 1, nchar(getwd()) - 13)                                  # going up 1 directory
  FileName = paste(period,implse,resp, sep = "_")
  path = paste(path,'Paper/Image_fromCode/',FileName,".png", sep = "")
  ggsave(filename = path, plot = IRFPlot, width = 5, height = 4, dpi = 300)
  return(IRFPlot)
}


# Creating plots ----------------------------------------------------------

IRF_GGplot = list()
for (i in 1:length(IRFs)){
  irfpic = irfCreate(i)
  IRF_GGplot = c(IRF_GGplot, list(irfpic) ) 
}
irfpic = irfCreate(14)
irfpic
