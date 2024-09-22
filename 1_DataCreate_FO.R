# This file uses Banxico data on value of  ownership of bonds by different categories, 
# calculates proportion owned by different groups. It exports a file with data on 
# foreign ownership.Also calculates some summary stats used in-text in the paper.

# Import data, create proportion, export FO --------------------------------

## Importing Ownership data ------------------------------------------------

Own_Data = read_xlsx("Descriptive/Mexico/Mex_GD_ownership.xlsx", range = "A18:L7799")
Own_Data$Date = as.Date(Own_Data$Date)
Own_Data[,-1] = lapply(Own_Data[,-1], as.numeric) 

data_desc = data.frame(code = colnames(Own_Data)[-1],Desc = names(
                         read_xlsx("Descriptive/Mexico/Mex_GD_ownership.xlsx",
                                              range = "B10:L10")))                #Importing variable names

## Finding start dates -----------------------------------------------------

a = !is.na(Own_Data[,-1])
first_value = NaN
for (i in 1:(ncol(Own_Data)-1)){
  first_value[i] = which(a[,i] == T)[1]                                           #this picks the row number when the first non NA value appears in each column
}
start_date_Own = data.frame(Var = colnames(Own_Data)[-1], 
                        Start = Own_Data$Date[first_value] )
rm(list = c('a', 'first_value'))
#most missing values is for SF235837 and SF65212, But these can be subsumed in Banxico
#ownership.

## Calculating Banxico ownership ------------------------------------------

Own_Data[is.na(Own_Data)] = 0                                                         #Converting NAs to 0 so that sum of NA and non-NA doesnt become NA
Own_Data$Banxico =  rowSums(Own_Data[,c("SF235837","SF65210","SF65212")], na.rm = F)  #Calculating Banxico holding as sum of components


# Calculating proportions owned  ------------------------------------------

## Exploring sum of shares > 1 ---------------------------------------------
# In the proportions calculated with the total from Banxico data as the 
# denominator, there are many that are slightly larger or smaller than 1(problem
# with rounding off components ?). There are a few where the difference is greater
# than 1%. This section explores the data to find how many have a sum different 
# from the data on Total and how big is the difference. Finally, proportions are
# calculated with the sum of components as denominator.

cols = c("SF235837","SF65210","SF65211","SF65212","SF65213","SF65214",
         "SF65215","SF65216","SF65218")
test = data.frame(Date = Own_Data$Date, Sum = rowSums(Own_Data[,cols]), 
                  Total = Own_Data$SF65219)
test$Diff = ((test$Sum/test$Total)- 1)*100
length(test$Date[test$Diff >= 1])
length(test$Date)
test[which.max(test$Diff),]

rm(test)

## Creating Proportion columns ---------------------------------------------
Own_Data$Sum = rowSums(Own_Data[,cols])
a = paste(colnames(Own_Data)[-1], "_p", sep = "")                                   #creating names for new columns
a = a[!a %in% "SF65219_p"]                                                        #removing column for total
Own_Data[a] = NA                                                                   #adding columns with NA
for (i in 1:length(a) ){
  Own_Data[,a[i]] = Own_Data[,substr(a[i],1,nchar(a[i])-2)]/Own_Data["Sum"]                 #calculating proportions within a loop
}


# Creating FO Data --------------------------------------------------------
Mex_FO = Own_Data[,c("Date","SF65218", "SF65218_p")]                              #Extracting foreign ownership data from all ownership data.
colnames(Mex_FO)[-1] = c("F_Own", "F_Own_p")
write.csv(Mex_FO, "Mexicopaper_Data/Mex_FO_daily.csv")                            #exporting file to be used in creating a common datatable to be used for SVAR



# FO analysis (optional) --------------------------------------------------

#calculating average foreign ownership in analysis periods (a useful descriptive statistic)
colMeans(Mex_FO[Mex_FO$Date >= as.Date("2006-09-01") & Mex_FO$Date <= as.Date("2008-07-31"),"F_Own_p"])
colMeans(Mex_FO[Mex_FO$Date >= as.Date("2014-01-01") & Mex_FO$Date <= as.Date("2015-12-31"),"F_Own_p"])
colMeans(Mex_FO[Mex_FO$Date >= as.Date("2010-01-01") & Mex_FO$Date <= as.Date("2011-12-31"),"F_Own_p"])
colMeans(Mex_FO[Mex_FO$Date >= as.Date("2012-01-01") & Mex_FO$Date <= as.Date("2013-12-31"),"F_Own_p"])

