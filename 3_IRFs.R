# this code uses the list 'IRFs' created by 2_SVAR and plots the IRFs for various
# period and variable specifications

# Function to create IRF -------------------


irfGen = function(samp_num,resp_num){
  # Logic of steps:
  # 1.Check if required subset of IRF data has NAs 
  # 2.Subset required data from list and store in format needed for plotting.
  # 3.Idenitfy name of response variable
  # 4. Plot IRF
  
  if ( all(is.na(IRFs[[samp_num]])) ){
    print('No analysis had been done owing to missing value')
    return()}
  
  irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$TIIE) - 1),
                      IRF = IRFs[[samp_num]]$irf$TIIE[,resp_num],
                      Upper_CI = IRFs[[samp_num]]$Upper$TIIE[,resp_num],
                      Lower_CI = IRFs[[samp_num]]$Lower$TIIE[,resp_num])
  
  
  # Before plotting I identify a long and short name for impulse variable and
  # response variable. long name is used to label figure and short name to save file
  
  # Identify response variable name from sample
  resp_var = colnames(IRFs[[samp_num]]$Upper$TIIE)[2]
  if (resp_var %in% c("MPTBA", "MPTBC", "MPTBF", "MPTBI") ){
    resp = substr(resp,5,5)
    if (resp == "A"){ mo = "1"}
    if (resp == "C"){ mo = "3"}
    if (resp == "F"){ mo = "6"}
    if (resp == "I"){ mo = "9"}
    resp = paste(mo,"mo", sep = "")
    Resp = paste(mo,"mo", sep = " ")
  }else{
    resp = paste(substr(resp_var,5,6),"y", sep = "")
    Resp = paste(substr(resp_var,5,6),"yr", sep = " ")
  }
  
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
  
  IRFPlot = ggplot(irfDat, aes(x = Week)) +
    geom_line(aes(y = IRF), color = "black", linewidth = 1.2) +                     # Main IRF line
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = 'blue', alpha = 0.3) +  # CI band
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +  # Zero line
    scale_y_continuous(limits = c(-0.4, 0.85), breaks = seq(-0.2, 0.8, by = 0.1)) +
    labs(title = paste("Impulse Response to", Implse, "yield"), x = "Week",
         y = paste('\u0394', Resp, "yield") ) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
  
  #Saving plot to file
  path = substr(getwd(), 1, nchar(getwd()) - 13)                                  # going up 1 directory
  FileName = paste(period,implse,resp, sep = "_")
  path = paste(path,'Paper/Image_fromCode/',FileName,".png", sep = "")
  ggsave(filename = path, plot = IRFPlot, width = 5, height = 4, dpi = 300)
 
  return(IRFPlot)
  
  
}


# Creating plots ----------------------------------------------------------

IRF_GGplot = list()
sampleNums = c(seq(1,49,3), seq(2,50,3) )

for (i in sampleNums){
  irfpic = irfGen(i,2)
  IRF_GGplot = c(IRF_GGplot, list(irfpic) ) 
}

