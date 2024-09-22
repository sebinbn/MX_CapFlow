# This code uses Mex_GD_Maturity.xlsx which has monthly data from Dec 2012 on 
# ownership of Mexican bonds classified by issuer, holder and remaining maturity.
#It uses the data to create Mex_mat


# Data Import -------------------------------------------------------------

BM_Mat = read_xlsx("Descriptive/Mexico/Mex_GD_Maturity.xlsx", range = "A18:YN153")
BM_Mat$Date = seq.Date(as.Date("2012-12-01"), as.Date("2024-02-01"), by = "month")

Mex_mat = BM_Mat[,c("Date","SF224646",	"SF224647",	"SF224648","SF224697",
                    "SF224698","SF224699","SF224748",	"SF224749",	"SF224750")]    #Extracting columns having ownership in Bn of Pesos split into long and short term
colnames(Mex_mat)[-1] = c("R_Own_Tot", "R_Own_short","R_Own_long","F_Own_Tot", 
                          "F_Own_short","F_Own_long", "GG_Tot","GG_short","GG_long")

# Creating new variables ---------------------------------------------------
Mex_mat$R_lt_share = Mex_mat$R_Own_long/Mex_mat$R_Own_Tot
Mex_mat$F_lt_share = Mex_mat$F_Own_long/Mex_mat$F_Own_Tot
Mex_mat$GG_lt_share = Mex_mat$GG_long/Mex_mat$GG_Tot

rm(BM_Mat)
