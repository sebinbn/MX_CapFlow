# This plots the yield curves over time being used for a motivation picture

library(plotly)

Mex_YC_dat = BBYield[BBYield$Date <= as.Date("2023-12-31") & 
            BBYield$Date >= as.Date("2008-04-01"), ]
Mex_YC_dat = Mex_YC_dat[c("Date", "MPTBA", "MPTBC", "MPTBF", "MPTBI",
                          colnames(BBYield)[2:14] )]
all_days <- seq.Date(from = as.Date("2008-04-01"), to = as.Date("2023-12-31"),
                     by = "day")
x <- data.frame(Date = all_days[!(weekdays(all_days) %in% c("Saturday", "Sunday"))] )
Mex_YC_dat = merge(x, Mex_YC_dat, by.x = "Date", all.x = T)
Mex_YC_dat[,-1] = na.approx(Mex_YC_dat[,-1], na.rm = F)

yc_cols = colnames(Mex_YC_dat[,-1])
tenor_labels <- substr(yc_cols , 5, nchar(yc_cols) )

library(dplyr)
library(tidyr)
yc_long <- Mex_YC_dat %>%
  pivot_longer(
    cols = all_of(yc_cols),
    names_to = "Tenor",
    values_to = "Yield"
  ) %>%
  mutate(
    Tenor = factor(Tenor, levels = yc_cols),
    TenorNum = as.numeric(Tenor)
  ) %>%
  arrange(Date, TenorNum)

# Axes
dates  <- unique(yc_long$Date)
tenors <- unique(yc_long$TenorNum)

# Yield matrix (Tenor × Date)
Z <- matrix(
  yc_long$Yield,
  nrow = length(tenors),
  ncol = length(dates),
  byrow = FALSE
)

# 3-D yield curve surface
plot_ly(
  x = dates,
  y = tenors,
  z = Z,
  type = "surface",
  colorscale = "Viridis",
  colorbar = list(
    title = list(text = "Yield", font = list(size = 16))
  )
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = list(text = "Date", font = list(size = 18))
      ),
      yaxis = list(
        title = list(text = "Tenor", font = list(size = 18)),
        tickvals = tenors,
        ticktext = tenor_labels
      ),
      zaxis = list(
        title = list(text = "Yield", font = list(size = 18))
      )
    )
  )

# 
# plot_ly(Mex_YC_dat,
#         x = Mex_YC_dat$Date,             
#         y = colnames(Mex_YC_dat)[-1],           
#         z = Mex_YC_dat[-1],               
#         type = "surface",
#         colorscale = "Viridis",
#         colorbar = list(title = list( text = "Cumulative IRF",font = list(size = 16)),
#                         tickfont = list(size = 14),len = 0.7, 
#                         x = 0.95, xanchor = "left")
# ) %>%
#   layout(
#     scene = list(
#       xaxis = list(title = list(text = "Date", font = list(size = 18)),
#                    tickfont = list(size = 14)),
#       yaxis = list(title = list(text ="Propn. of FO Bonds",  font = list(size = 18)),
#                    tickfont = list(size = 14)),
#       zaxis = list(title = list(text ="Cumulative IRF", font = list(size = 18)),
#                    tickfont = list(size = 14))     
#     )  )
