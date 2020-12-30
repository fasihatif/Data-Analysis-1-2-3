##########################################
#               Load libraries           #
##########################################

library(tidyverse)
library(data.table)
library(lubridate)
library(fastDummies)
library(estimatr)
library(scales)
library(faraway)
library(caret)
library(car)
library(corrplot)

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
write.csv(df_draft, "df_draft.csv")

rm(DfChurnOutput,DfCustomerHistory)

# Get stats of each column
summary(df_draft) # add some analysis regarding consumption and forecasting


##########################################
#             Cleaning the Data          #
##########################################

# Number of missing values in each column
na_count <- sapply(df_draft, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Number of missing values in percent in each column. 
# We will check which columns have missing values greater than 30% and drop them. We will also ensure that the columns to be dropped dont contain any important information inw hich case we wont drop them.
na_count$na_percent <- sapply(df_draft, function(y) round((sum(length(which(is.na(y))))/length(y))*100.00,2))

# Save the names of columns which have missing values less than 30%
col_30 <- na_count %>% filter(na_percent < 30)
col_30 <- rownames(col_30)

# Remove columns with NA values greater than 30%
df_draft <- df_draft %>% select(all_of(col_30))

# Fill in missing dates with median
## Replace NA values in 'date_end' column with median date
df_draft$date_end[is.na(df_draft$date_end)]<-median(df_draft$date_end,na.rm=TRUE)

##Replace NA values in 'date_renewal' column with median date
df_draft$date_renewal[is.na(df_draft$date_renewal)]<-median(df_draft$date_renewal,na.rm=TRUE)

## Replace NA values in 'date_modif_prod' column with median date
df_draft$date_modif_prod[is.na(df_draft$date_modif_prod)]<-median(df_draft$date_modif_prod,na.rm=TRUE)

## Replace NA values in channel sales column with 'null_channel'
df_draft$channel_sales[is.na(df_draft$channel_sales)] <- "null_channel"

# Convert 'has_gas' column from T/F to 1/0
df_draft <- df_draft %>% mutate('has_gas' =  as.numeric(has_gas))

##########################################
#            Feature Engineering         #
##########################################

# Create 'contract duration' column and divide by 365 to get yearly values
df_draft <- df_draft %>% mutate('contract_duration' =  as.integer((difftime(date_end,date_activ, unit = "days"))/(365.25/12)))

# Create reference date for calculations. We will take 1st Jan 2020 since it is given as the reference date
ref_date = ymd(20160101)

# No of years since contract went active
df_draft <- df_draft %>% mutate('months_active' =  as.integer((difftime(ref_date,date_activ, unit = "days"))/(365.25/12)))

# No of years left in contract
df_draft <- df_draft %>% mutate('months_end' =  as.integer((difftime(date_end,ref_date, unit = "days"))/(365.25/12)))

# No of years since last modification at reference date
df_draft <- df_draft %>% mutate('months_modif' =  as.integer((difftime(ref_date,date_modif_prod, unit = "days"))/(365.25/12)))

# Number of months since last renewal at reference date
df_draft <- df_draft %>% mutate('months_renewal' =  as.integer((difftime(ref_date,date_renewal, unit = "days"))/(365.25/12)))

# Remove columns with NA values greater than 30%
df <- df_draft %>% select(all_of(col_30), contract_duration,has_gas,months_active,months_end,months_modif,months_renewal)
df <- df %>% select(-c(date_activ,date_end,date_modif_prod,date_renewal, date_renewal,forecast_cons_year, origin_up))

# Dummy Variable creation for channel_sales and has_gas
## How many dummy columns to make?
# df_backup <- df

df %>% 
  group_by(channel_sales) %>%
  summarize(channel_count = n()) # 8 unique channels so we will make 8 dummy columns

## Create dummy variables using the fastdummies library
df <- df %>% dummy_cols(select_columns = c("channel_sales","has_gas"), remove_selected_columns = TRUE)

## Updating column names of dummy variables for easier understanding
df <- df %>% 
  rename(
    "channel_foos" = channel_sales_foosdfpfkusacimwkcsosbicdxkicaua,
    "channel_usil" = channel_sales_usilxuppasemubllopkaafesmlibmsdf,
    "channel_lmke" = channel_sales_lmkebamcaaclubfxadlmueccxoimlema,
    "channel_ewpa" = channel_sales_ewpakwlliwisiwduibdlfmalxowmwpci,
    "channel_epum" = channel_sales_epumfxlbckeskwekxbiuasklxalciiuu,
    "channel_sddi" = channel_sales_sddiedcslfslkckwlfkdpoeeailfpeds,
    "channel_fixd" = channel_sales_fixdbufsefwooaasfcxdxadsiekoceaa,
    "channel_null" = channel_sales_null_channel
  )

## Remove one of the dummy columns to cater for multicollinearity. We will remove 'channel_null' and 'has_gas_0'
df <- subset(df,select = -c(channel_null,has_gas_0))

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


# Function to calculate no of months in the duration between a date column and reference date
months_length <- function(column1, column2){
  
  dataframe <- data.frame(column1, column2)
  
  bar_chart <- dataframe %>%
    group_by(column1, column2) %>%
    summarise(months_count = n()) %>%
    mutate(status = ifelse(column2 == 1, "Churned", "Retention")) %>%
    ggplot(aes(x = column1, y = months_count, fill= factor(status))) +
    geom_bar(stat = "identity") +
    labs(x = "No of months*", y = "No of Companies", fill = "Status", caption = "*Reference date taken as 1st Jan 2020")
  
  return(bar_chart)
}

# Duration of contract
contract_duration_barchart <- months_length(df$contract_duration,df$churn)

# No of months passed from contract active date to reference date
months_active_barchart <- months_length(df$months_active,df$churn)

# No of months left till end date from reference date
months_end_barchart <- months_length(df$months_end,df$churn)

# No of months left till renewal from reference date
months_renewal_barchart <- months_length(df$months_renewal,df$churn)

# No of months since contract was last modified 
months_modif_barchart <- months_length(df$months_modif,df$churn)

##### CONSUMPTION VARIABLES EXPLORATORY ANALYSIS #####

df %>%
  select(cons_12m,cons_gas_12m,cons_last_month,imp_cons) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() + theme_bw()


summary(df_draft$cons_last_month)

##### FORECAST VARIABLES EXPLORATORY ANALYSIS #####

forecast_eda <- df %>%
  select(forecast_cons_12m,forecast_discount_energy,forecast_meter_rent_12m,forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() + theme_bw()

##### MARGIN VARIABLES EXPLORATORY ANALYSIS #####

margin_eda <- df %>%
  select(margin_gross_pow_ele,margin_net_pow_ele,net_margin) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() + theme_bw()

##### OTHER VARIABLES EXPLORATORY ANALYSIS #####

other_eda <- df %>%
  select(nb_prod_act,num_years_antig,pow_max) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() + theme_bw()

##########################################
#        Transformation of data          #
##########################################
# backupdf1 <- df

##### Transformation of Consumption variables #####

# Consumption variables are right skewed as we saw from the histograms. To make them normally distributed, we will
# take log of these variables. However, these 4 variables include negative and zero values for which log cant be taken.So 
# we will convert negative values to NaN and add a constant 1 to these variables as well

# Set negative values as NaN as log cant be taken for negative values
df <- df %>% mutate(cons_12m = replace(cons_12m, which(cons_12m < 0), NaN))
df <- df %>% mutate(cons_gas_12m = replace(cons_gas_12m, which(cons_gas_12m < 0), NaN))
df <- df %>% mutate(cons_last_month = replace(cons_last_month, which(cons_last_month < 0), NaN))
df <- df %>% mutate(imp_cons = replace(imp_cons , which(imp_cons  < 0), NaN))

# Add constant 1 to the variables and then take log
df <- df %>% mutate( ln_cons_12m = log( cons_12m + 1 ),
                     ln_cons_gas_12m = log( cons_gas_12m + 1),
                     ln_cons_last_month = log(cons_last_month + 1),
                     ln_imp_cons = log(imp_cons + 1)) 

summary(df$cons_12m)

##### Transformation of Forecast variables #####

# Set negative values as NaN as log cant be taken for negative values
df <- df %>% mutate(forecast_cons_12m = replace(forecast_cons_12m, which(forecast_cons_12m < 0), NaN))
df <- df %>% mutate(forecast_meter_rent_12m = replace(forecast_meter_rent_12m, which(forecast_meter_rent_12m < 0), NaN))


# Add constant 1 to the variables and then take log
df <- df %>% mutate( ln_forecast_cons_12m = log(forecast_cons_12m + 1),
                     ln_forecast_meter_rent_12m = log(forecast_meter_rent_12m + 1)) 

# backup2 <- df

# Now check for the distribution of the transformed variables
transformed_eda <- df %>%
  select(ln_cons_12m,ln_cons_gas_12m,ln_cons_last_month,ln_imp_cons,ln_forecast_cons_12m,ln_forecast_meter_rent_12m) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() + theme_bw()

##########################################
#          Further Data Cleaning         #
##########################################

# Checkpoint for further data cleaning
# df_ftc <- df
# df <- df_ftc

df <- subset(df,select = -c(id,cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))

remove_outliers <- function(column_name){
  
  outliers <- boxplot(column_name, plot=FALSE)$out
  df<- df[-which(column_name %in% outliers),]
}

boxplot(df$forecast_price_energy_p1) 
remove_outliers(df$forecast_price_energy_p1)
boxplot(df$forecast_price_energy_p1)

boxplot(df$forecast_price_energy_p2)
remove_outliers(df$forecast_price_energy_p2)
boxplot(df$forecast_price_energy_p2)

boxplot(df$forecast_discount_energy)
remove_outliers(df$forecast_discount_energy)
boxplot(df$forecast_discount_energy)

boxplot(df$forecast_price_pow_p1)
remove_outliers(df$forecast_price_pow_p1)
boxplot(df$forecast_price_pow_p1)

boxplot(df$ln_forecast_cons_12m)
remove_outliers(df$ln_forecast_cons_12m)
boxplot(df$ln_forecast_cons_12m)

boxplot(df$ln_forecast_meter_rent_12m)
remove_outliers(df$ln_forecast_meter_rent_12m)
boxplot(df$ln_forecast_meter_rent_12m)

boxplot(df$pow_max)
remove_outliers(df$pow_max)
boxplot(df$pow_max)

boxplot(df$ln_cons_12m)
remove_outliers(df$ln_cons_12m)
boxplot(df$ln_cons_12m)

boxplot(df$ln_cons_gas_12m)
remove_outliers(df$ln_cons_gas_12m)
boxplot(df$ln_cons_gas_12m)

boxplot(df$ln_cons_last_month)
remove_outliers(df$ln_cons_last_month)
boxplot(df$ln_cons_last_month)

boxplot(df$ln_imp_cons)
remove_outliers(df$ln_imp_cons)
boxplot(df$ln_imp_cons)

boxplot(df$margin_gross_pow_ele)
remove_outliers(df$margin_gross_pow_ele)
boxplot(df$margin_gross_pow_ele)

boxplot(df$margin_net_pow_ele)
remove_outliers(df$margin_net_pow_ele)
boxplot(df$margin_net_pow_ele)

boxplot(df$net_margin)
remove_outliers(df$net_margin)
boxplot(df$net_margin)

boxplot(f$nb_prod_act)
remove_outliers(df$nb_prod_act)
boxplot(f$nb_prod_act)

boxplot(df$months_active)
remove_outliers(df$months_active)
boxplot(df$months_active)

boxplot(df$months_end)
remove_outliers(df$months_end)
boxplot(df$months_end)

boxplot(df$months_modif)
remove_outliers(df$months_modif)
boxplot(df$months_modif)

boxplot(df$months_renewal)
remove_outliers(df$months_renewal)
boxplot(df$months_renewal)

?read.csv

##### Correlation Matrix #####

# When you have two independent variables that are very highly correlated, you definitely should remove one of them
# because you run into the multicollinearity conundrum and your regression model's regression coefficients related to 
# the two highly correlated variables will be unreliable

# We would like to check for multicollinearity between the explanatory variables. For this, we will first create the 
# correlation matrix. For pairs that have very high correlations, we will use Variance Inflation Factor to determine which
# variable to remove. We will take a relaxed VIF value of 10 as a threshold. We will remove all variables which have a VIF
# than 10

# Remove variables that have already been transformed and no longer required. We will assign to new dataframe so we have original as backup
# and can always come back to play with it
df_ml <- subset(df,select = -c(id,cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))

# Correlation Matrix
cor1 <- cor(df_ml, use = "pairwise.complete.obs")
ggcorrplot::ggcorrplot(cor1, method = "square",lab = TRUE)

# From the correlation matrix, we can see that 'contract_duration', 'month_activ' and 'num_years_antig' and 'months_end' have the highest correlation
# Calculate Variance Inflation Factor using the 'car' package
lm_model <- lm(churn ~ ., data = df_ml)
car::vif(lm(lm_model))

# From the VIF we can confirm that the correlations are very high and we can drop one of the variables from each correlation pair.
df_ml <- subset(df_ml, select = -c(contract_duration,num_years_antig))

##########################################
#             Machine Learning           #
##########################################

##### SPLIT DATA INTO TEST/TRAIN DATASET #####

intrain<- createDataPartition(df_ml$churn,p=0.7,list=FALSE)
set.seed(2017)
train <- df_ml[intrain,]
test <- df_ml[-intrain,]

##### LOGISTIC REGRESSION VIA GLM() #####

glm_model <- glm(churn ~ ., data = df_ml, family = binomial("logit"))
summary(glm_model)

# df_test <- subset(df, select = c(forecast_discount_energy,forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1,margin_gross_pow_ele,margin_net_pow_ele,nb_prod_act,net_margin,num_years_antig,pow_max,churn,contract_duration,months_active,months_end,months_modif,months_renewal,channel_epum,channel_ewpa,channel_fixd,channel_foos,channel_lmke,channel_sddi,channel_usil,has_gas_1,ln_cons_12m,ln_cons_gas_12m,ln_cons_last_month,ln_imp_cons,ln_forecast_cons_12m,ln_forecast_meter_rent_12m))
# glm_model <- lm(churn~., data = df, family = binomial("logit"),control=glm.control(maxit=100))

##### LOGISTIC REGRESSION VIA CARET #####

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
glm_model_caret <- train(factor(churn)~.,
                         data = df_test,
                         trControl = train_control,
                         method = "glm",
                         na.action = na.pass,
                         family=binomial())

# print cv scores
summary(glm_model_caret)


na_test <- data.frame(sapply(df_out, function(y) sum(length(which(is.na(y))))))
a <- boxplot(df_out$ln_cons_12m)$out

df_out$ln_cons_12m[df_out$ln_cons_12m %in% boxplot(df)$out] <- mean(df_out$ln_cons_12m)
str(df_out)

######################################################################







