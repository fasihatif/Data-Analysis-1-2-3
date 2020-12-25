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
#         Understanding the Data         #
##########################################

# Look at first 5 rows of each table
head(DfChurnOutput)
head(DfCustomerHistory)
head(DfPricingHistory)

# Look at datatype of columns in the table
glimpse(DfChurnOutput)
glimpse(DfCustomerHistory)
glimpse(DfPricingHistory)

# Number of rows in DfCustomerHistory
count(unique(DfCustomerHistory)) #16096

# Number of rows in DfChurnOutput
count(unique(DfChurnOutput)) #16096

# Merge dataframes by id column since equal and unique ids
DfCustomerHistory <- merge(DfCustomerHistory,DfChurnOutput, by = 'id')
# Calculate number and percentage of missing values in each column

# Get stats of each column
summary(DfCustomerHistory) # add some analysis regarding consumption and forecasting

# Number of missing values in each column
na_count <- sapply(DfCustomerHistory, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Number of missing values in percent in each column. 
# We will check which columns have missing values greater than 30% and drop them. We will also ensure that the columns to be dropped dont contain any important information inw hich case we wont drop them.
na_count$na_percent <- sapply(DfCustomerHistory, function(y) round((sum(length(which(is.na(y))))/length(y))*100.00,2))

# Save the names of columns which have missing values less than 30%
col_30 <- na_count %>% filter(na_percent < 30)
col_30 <- rownames(col_30)

##########################################
#             Feature Engineering        #
##########################################

# Create 'contract duration' column and divide by 365 to get yearly values
DfCustomerHistory <- DfCustomerHistory %>% mutate('contract_duration' =  round(as.numeric(difftime(date_end,date_activ, unit = "days"))/365, digits = 2))

# Convert 'has_gas' column from T/F to 1/0
DfCustomerHistory <- DfCustomerHistory %>% mutate('has_gas' =  as.numeric(has_gas))

# Create contract_modif column
DfCustomerHistory <- DfCustomerHistory %>% mutate('contract_modif' =  ifelse(date_modif_prod>date_activ,1,0))

# Create final dataset for analysis
df <- DfCustomerHistory %>% select(c(col_30,contract_duration,has_gas,contract_modif))





