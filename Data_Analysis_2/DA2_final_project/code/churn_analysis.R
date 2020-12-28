##########################################
#               Load libraries           #
##########################################

library(tidyverse)
library(data.table)
library(lubridate)


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

# Merge dataframes by id column since equal and has unique ids. We create a df named df_draft where we do all the working for cleaning and feature engineering
df_draft <- merge(DfCustomerHistory,DfChurnOutput, by = 'id')
# Calculate number and percentage of missing values in each column

# Get stats of each column
summary(df_draft) # add some analysis regarding consumption and forecasting

# Number of missing values in each column
na_count <- sapply(df_draft, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Number of missing values in percent in each column. 
# We will check which columns have missing values greater than 30% and drop them. We will also ensure that the columns to be dropped dont contain any important information inw hich case we wont drop them.
na_count$na_percent <- sapply(df_draft, function(y) round((sum(length(which(is.na(y))))/length(y))*100.00,2))

# Save the names of columns which have missing values less than 30%
col_30 <- na_count %>% filter(na_percent < 30)
col_30 <- rownames(col_30)

##########################################
#             Cleaning the Data          #
##########################################

# Remove columns with NA values greater than 30%
df_draft <- df_draft %>% select(all_of(col_30))

# Fill in missing dates with median
## Replace NA in 'date_end' column with median date
df_draft$date_end[is.na(df_draft$date_end)]<-median(df_draft$date_end,na.rm=TRUE)

##Replace NA in 'date_renewal' column with median date
df_draft$date_renewal[is.na(df_draft$date_renewal)]<-median(df_draft$date_renewal,na.rm=TRUE)

## Replace NA in 'date_modif_prod' column with median date
df_draft$date_modif_prod[is.na(df_draft$date_modif_prod)]<-median(df_draft$date_modif_prod,na.rm=TRUE)

# Convert 'has_gas' column from T/F to 1/0
df_draft <- df_draft %>% mutate('has_gas' =  as.numeric(has_gas))

##########################################
#            Feature Engineering         #
##########################################

# Create 'contract duration' column and divide by 365 to get yearly values
df_draft <- df_draft %>% mutate('contract_duration' =  round(as.numeric(difftime(date_end,date_activ, unit = "days"))/365, digits = 2))


# Create contract_modif column
df_draft <- df_draft %>% mutate('contract_modif' =  ifelse(date_modif_prod>date_activ,1,0))

# Create reference date for calculations. We will take 1st Jan 2020 since it is fiven as the reference date
ref_date = ymd(20160101)

# No of years since contract went active
df_draft <- df_draft %>% mutate('years_active' =  round(as.numeric(difftime(ref_date,date_activ, unit = "days"))/365, digits = 2))

# No of years left in contract
df_draft <- df_draft %>% mutate('years_end' =  round(as.numeric(difftime(date_end, ref_date, unit = "days"))/365, digits = 2))

# No of years since last modification at reference date
df_draft <- df_draft %>% mutate('years_modif' =  round(as.numeric(difftime(ref_date,date_modif_prod, unit = "days"))/365, digits = 2))

# Number of months since last renewal at reference date since last renewal at reference date
df_draft <- df_draft %>% mutate('years_renewal' =  round(as.numeric(difftime(ref_date,date_renewal, unit = "days"))/365, digits = 2))


# How much was last months power consumption lesser/greater than the average consumption of the last 12 months?
df_draft <- df_draft %>% mutate("power_lm_vs_avg" = cons_last_month - mean(cons_12m))

# Remove columns with NA values greater than 30%
#df <- df_draft %>% select(all_of(col_30),power_lm_vs_avg, contract_duration,has_gas,contract_modif)
df <- df_draft %>% select(-c(channel_sales,cons_12m, cons_last_month, date_activ,date_end,date_modif_prod,date_renewal, date_renewal,forecast_cons_year, origin_up))

##########################################
#        Exploratory Data Analysis       #
##########################################

# Churn Rate
churn_rate_barchart <- df %>% 
  select(churn) %>%
  group_by(churn) %>%
  summarise(percentage = n()) %>%
  mutate(Percent = round(100*percentage/sum(percentage),1)) %>%
  mutate(status = ifelse(churn == 1, "Churned", "Retention")) %>%
  ggplot(aes(x = "Companies", y = Percent, fill= factor(status, levels=c("Churned","Retention")))) +
  geom_bar(stat = "identity") + geom_text(aes(label = Percent), position = position_stack(vjust = .5)) +
  labs(x = '', fill = "Status")


# Contract duration of companies
## Creating bins for years
breaks <- c(1:17)
tags <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)

df$duration_bins <- cut(df$contract_duration, 
                        breaks=breaks, 
                        include.lowest=TRUE, 
                        right=FALSE, 
                        labels=tags)

duration_barchart <- df %>% 
  select(churn,contract_duration,duration_bins) %>%
  group_by(duration_bins,churn) %>%
  summarise(duration_count = n()) %>%
  mutate(status = ifelse(churn == 1, "Churned", "Retention")) %>%
  ggplot(aes(x = duration_bins, y = duration_count, fill= factor(status))) +
  geom_bar(stat = "identity") +
  labs(x = "Contract Duration (Years)", y = "No of Companies", fill = "Status")





# Correlation
select_cols <- df %>% select(forecast_meter_rent_12m,forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1, has_gas, imp_cons)
cor1 <- cor(select_cols, use = "pairwise.complete.obs")
ggcorrplot::ggcorrplot(cor1, method = "square",lab = TRUE)





