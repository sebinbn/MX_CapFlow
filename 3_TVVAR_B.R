###############################################################
# 
# This script generates five ggplot visualizations showing how
# the estimated coefficients in Phi matrix (B matrix in tvVAR fn) 
# (TIIE.l1 to TIIE.l5) vary over FO.
#

# Date: Oct-08-2025
###############################################################
load("SavedRResults/tvVARResults_Oct17.RData")
obj =  CumIRF_1mo
# --- Prepare data for plotting ---
df <- data.frame( FO_p = obj$x$z,
                  obj$x$coefficients$MPTBA[,
                    c("TIIE.l1","TIIE.l2","TIIE.l3","TIIE.l4","TIIE.l5")] )
  
# Function to create ggplot for each lag
make_plot <- function(data, var_name) {
  ggplot(data, aes(x = FO_p, y = .data[[var_name]])) +
    geom_point(size = 1) +
    scale_y_continuous(limits = c(-0.06,0.11))+
    theme_minimal() +
    theme(plot.title = element_text(size = 16)) +
    labs(x = "Propn. of FO bonds", y = element_blank(),
         title = paste("Lag -",
                       substr(var_name,nchar(var_name),nchar(var_name) )) )
}

# Create 5 individual plots
p1 <- make_plot(df, "TIIE.l1")
p2 <- make_plot(df, "TIIE.l2")
p3 <- make_plot(df, "TIIE.l3")
p4 <- make_plot(df, "TIIE.l4")
p5 <- make_plot(df, "TIIE.l5")


# Arrange plots in a 2x3 grid (last cell is legend)
final_plot <- ggarrange(p1, p2, p3, p4, p5, ncol = 2, nrow = 3 )
