# this code uses the list 'IRFs' created by 2_SVAR and plots the IRFs for various
# period and variable specifications

# Function to create data needed for IRF as data frame -------------------

irfDataPrep = function(samp_num){
  if(samp_num <= 3){
    irfDat = data.frame(Week = 0:(nrow(IRFs[[samp_num]]$irf$MPTBA) - 1),
                        IRF = IRFs[[samp_num]]$irf$MPTBA[,'GMXN10Y'],
                        Upper_CI = IRFs[[samp_num]]$Upper$MPTBA[,'GMXN10Y'],
                        Lower_CI = IRFs[[samp_num]]$Lower$MPTBA[,'GMXN10Y'])
  }else if(samp_num >= 7){
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
}

# Function to plot IRF using ggplot ---------------------------------------

irfPlot = function(Data, Impulse, Response){
  ggplot(Data, aes(x = Week)) +
    geom_line(aes(y = IRF), color = "black", linewidth = 1.2) +                     # Main IRF line
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = 'blue', alpha = 0.3) +  # CI band
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +  # Zero line
    labs(title = paste("Impulse Response to", Impulse), x = "Week",
         y = paste('\u0394', Response) ) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
}


# Creating plots ----------------------------------------------------------


## 1mo yield as short rate -------------------------------------------------
### 2010-11  -----------------------
irfDat = irfDataPrep(1)
IRF_1011_1mo = irfPlot(irfDat, '1 mo yield', '10 yr yield')
IRF_1011_1mo

### 2012-13 ----------------------------------------------
irfDat = irfDataPrep(2)
IRF_1213_1mo = irfPlot(irfDat, '1 mo yield', '10 yr yield')
IRF_1213_1mo

### 2014-15 ----------------------------------------------
irfDat = irfDataPrep(3)
IRF_1415_1mo = irfPlot(irfDat, '1 mo yield', '10 yr yield')
IRF_1415_1mo

## TIIE as short rate -------------------------------------------------
## 2010-11 -----------------------
irfDat = irfDataPrep(4)
IRF_1011_ON = irfPlot(irfDat, 'Overnight rate', '10 yr yield')
IRF_1011_ON

## 2012-13 ---------------------------------------------
irfDat = irfDataPrep(5)
IRF_1213_ON = irfPlot(irfDat, 'Overnight rate', '10 yr yield')
IRF_1213_ON

## 2014-15 ----------------------------------------------
irfDat = irfDataPrep(6)
IRF_1415_ON = irfPlot(irfDat, 'Overnight rate', '10 yr yield')
IRF_1415_ON

## TIIE as short rate, 1mo as long --------------------------------------------
## 2010-11  -----------------------
irfDat = irfDataPrep(7)
IRF_1011_ON1mo = irfPlot(irfDat, 'Overnight rate', '1 mo yield')
IRF_1011_ON1mo

## 2012-13 ----------------------------------------------
irfDat = irfDataPrep(8)
IRF_1213_ON1mo = irfPlot(irfDat, 'Overnight rate', '1 mo yield')
IRF_1213_ON1mo

## 2014-15 ----------------------------------------------
irfDat = irfDataPrep(9)
IRF_1415_ON1mo = irfPlot(irfDat, 'Overnight rate', '1 mo yield')
IRF_1415_ON1mo
