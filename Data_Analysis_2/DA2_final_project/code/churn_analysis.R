##########################################
#               Load libraries           #
##########################################

library(tidyverse)
library(data.table)


##########################################
#             Data Import                #
##########################################


# Import CustomerHistory.csv
urlCustomerHistory <- 'https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/DA2_final_project/data/CustomerDetails.csv'
DfCustomerHistory <- read_csv(urlCustomerHistory)

# Import PricingHistory.csv
urlPricingHistory <- 'https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/DA2_final_project/data/PricingHistory.csv'
DfPricingHistory <- read_csv(urlPricingHistory)

# Import ChurnOutput.csv
urlChurnOutput <- 'https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/DA2_final_project/data/ChurnOutput.csv'
DfChurnOutput <- read_csv(urlChurnOutput)


##########################################
#             Data Cleaning              #
##########################################

# Number of rows in DfCustomerHistory
count(unique(DfCustomerHistory)) #16096

# Number of rows in DfChurnOutput
count(unique(DfChurnOutput)) #16096

# Merge dataframes by id column
DfCustChurn <- merge(DfCustomerHistory,DfChurnOutput, by = 'id')

typeof(DfCustomerHistory)
