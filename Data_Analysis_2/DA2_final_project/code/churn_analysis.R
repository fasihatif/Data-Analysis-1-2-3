##########################################
#               Load libraries           #
##########################################

library(tidyverse)
library(data.table)
library(lubridate)
library(fastDummies)   # To create fummy variables
library(estimatr)
library(scales)        # To take logs of x and y variables
library(faraway)
library(caret)
library(car)           # To calculate VIF
library(corrplot)      # To draw correlation chart
library(outForest)
library(xgboost)
library(Ckmeans.1d.dp) # Required for xgb.ggplot.importance function in xgboost
library(randomForest)
library(Metrics)       # To calculate AUC
library(knitr)
library(ggcorplot)
library(ROCR)

##########################################
#             Data Import                #
##########################################

# Import CustomerHistory.csv
urlCustomerHistory <- 'https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/DA2_final_project/data/CustomerDetails.csv'
DfCustomerHistory <- read_csv(urlCustomerHistory)

# Import ChurnOutput.csv
urlChurnOutput <- 'https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/DA2_final_project/data/ChurnOutput.csv'
DfChurnOutput <- read_csv(urlChurnOutput)

##########################################
#         Understanding the Data         #
##########################################

# Look at first 5 rows of each table
head(DfChurnOutput)
head(DfCustomerHistory)

# Look at datatype of columns in the table
glimpse(DfChurnOutput)
glimpse(DfCustomerHistory)

# Number of rows in DfCustomerHistory
count(unique(DfCustomerHistory)) #16096

# Number of rows in DfChurnOutput
count(unique(DfChurnOutput)) #16096

# Merge dataframes by id column since equal and has unique ids. We create a df named df_draft where we do all the working for cleaning and feature engineering
df_draft <- merge(DfCustomerHistory,DfChurnOutput, by = 'id')
write.csv(df_draft, "df_draft.csv")

df%>% group_by(churn) %>%
  summarize(count = n())

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
    geom_bar(stat = "identity") + theme_bw() +
    labs(x = NULL, y = NULL,fill = "Status")
  
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

color <- "cyan3"
consumption_eda <- df %>%
  select(cons_12m,cons_gas_12m,cons_last_month,imp_cons) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = color) + theme_bw() +labs(y = NULL, x = NULL)


##### FORECAST VARIABLES EXPLORATORY ANALYSIS #####

forecast_eda <- df %>%
  select(forecast_cons_12m,forecast_discount_energy,forecast_meter_rent_12m,forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = color) + theme_bw() +labs(y = NULL, x = NULL)

##### MARGIN VARIABLES EXPLORATORY ANALYSIS #####

margin_eda <- df %>%
  select(margin_gross_pow_ele,margin_net_pow_ele,net_margin) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = color) + theme_bw() +labs(y = NULL, x = NULL)

##### OTHER VARIABLES EXPLORATORY ANALYSIS #####

other_eda <- df %>%
  select(nb_prod_act,num_years_antig,pow_max) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = color) + theme_bw() +labs(y = NULL, x = NULL)


# Non Parametric Charts
# cons_12m
cons_12m_sc <- ggplot(df , aes(x = cons_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "cons_12m",y = "Churn", title = "Level - Level for churn~cons_12m") 

ln_cons_12m_sc <- ggplot(df , aes(x = cons_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "cons_12m, ln scale",y = "Churn", title = "Level - log for churn~ln_cons_12m") + 
  scale_x_continuous(trans = log_trans())

# cons_last_month
cons_last_month_sc <- ggplot(df , aes(x = cons_last_month, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "cons_last_month",y = "Churn", title = "Level - Level for churn~cons_last_month") 

ln_cons_last_month_sc <- ggplot(df , aes(x = cons_last_month, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "cons_last_month, ln scale",y = "Churn", title = "Level - log for churn~ln_cons_last_month") + 
  scale_x_continuous(trans = log_trans())

# imp_cons
imp_cons_sc <- ggplot(df , aes(x = imp_cons, y = churn)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x = "imp_cons",y = "Churn", title = "Level - Level for churn~imp_cons") 

ln_imp_cons_sc <- ggplot(df , aes(x = imp_cons, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "imp_cons, ln scale",y = "Churn", title = "Level - log for churn~ln_imp_cons") + 
  scale_x_continuous(trans = log_trans())

# cons_gas_12m
cons_gas_12m_sc <- ggplot(df , aes(x = cons_gas_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x = "cons_gas_12m",y = "Churn", title = "Level - Level for churn~cons_gas_12m") 

ln_cons_gas_12m_sc <- ggplot(df , aes(x = cons_gas_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "cons_gas_12m, ln scale",y = "Churn", title = "Level - log for churn~ln_cons_gas_12m") + 
  scale_x_continuous(trans = log_trans())

# forecast_cons_12m
forecast_cons_12m_sc <- ggplot(df , aes(x = forecast_cons_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x = "forecast_cons_12m",y = "Churn", title = "Level - Level for churn~forecast_cons_12m") 

ln_forecast_cons_12m_sc <- ggplot(df , aes(x = cons_gas_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "forecast_cons_12m, ln scale",y = "Churn", title = "Level - log for churn~ln_forecast_cons_12m") + 
  scale_x_continuous(trans = log_trans())

# forecast_meter_rent_12m
forecast_meter_rent_12m_sc <- ggplot(df , aes(x = forecast_meter_rent_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x = "forecast_meter_rent_12m",y = "Churn", title = "Level - Level for churn~forecast_meter_rent_12m") 

ln_forecast_meter_rent_12m_sc <- ggplot(df , aes(x = forecast_meter_rent_12m, y = churn)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "forecast_meter_rent_12m, ln scale",y = "Churn", title = "Level - log for churn~ln_forecast_meter_rent_12m") + 
  scale_x_continuous(trans = log_trans())

##########################################
#          Further Data Cleaning         #
##########################################

# Checkpoint for further data cleaning
df_ftc <- df
# df <- df_ftc

# Subsetting important variables for outlier treatment
# df <- subset(df,select = -c(id,cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))
df_other <- df %>% select(-c(forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1,margin_gross_pow_ele,margin_net_pow_ele,net_margin,pow_max,cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))
df <- df %>% select(c(forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1,margin_gross_pow_ele,margin_net_pow_ele,net_margin,pow_max,cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))

# Detecting outliers and replacing them with mean
remove_outliers <- function(column_name){
  outliers <- boxplot(column_name, plot=FALSE)$out
  if(length(outliers) == 0){ column_name  <- column_name} else{
    column_name[column_name %in% outliers] = mean(column_name,na.rm = TRUE); column_name
  }
}

# Application of outlier function
df <- data.frame(sapply(df, remove_outliers))

# Join the treated dataset with rest of variables
df <- cbind(df_other,df)

# Continue on with df dataframe and treat it for missing values
# Remove all NA values and replace with mean

df_id <- df %>% select(id)
df <- df %>% select(-id)

# Save to new dataframe 'xgb_data'. The NA values in this table wont be treated for missing values as XGBoost can handle missing values internally
df_xgb <- df

rm(df_other)

##########################################
#        Transformation of data          #
##########################################
df_logs <- df
# df <- df_logs
##### Transformation of Consumption variables #####

# Consumption variables are right skewed as we saw from the histograms. To make them normally distributed, we will
# take log of these variables. However, these 4 variables include negative and zero values for which log cant be taken.So 
# we will convert negative values to NaN and add a constant 1 to these variables as well

# Set negative values as Na as log cant be taken for negative values
df <- df %>% mutate(cons_12m = replace(cons_12m, which(cons_12m < 0), NA))
df <- df %>% mutate(cons_gas_12m = replace(cons_gas_12m, which(cons_gas_12m < 0), NA))
df <- df %>% mutate(cons_last_month = replace(cons_last_month, which(cons_last_month < 0), NA))
df <- df %>% mutate(imp_cons = replace(imp_cons , which(imp_cons  < 0), NA))

# Add constant 1 to the variables and then take log since some values are zero
df <- df %>% mutate( ln_cons_12m = log( cons_12m + 1 ),
                     ln_cons_gas_12m = log( cons_gas_12m + 1),
                     ln_cons_last_month = log(cons_last_month + 1),
                     ln_imp_cons = log(imp_cons + 1)) 


##### Transformation of Forecast variables #####

# Set negative values as NaN as log cant be taken for negative values
df <- df %>% mutate(forecast_cons_12m = replace(forecast_cons_12m, which(forecast_cons_12m < 0), NA))
df <- df %>% mutate(forecast_meter_rent_12m = replace(forecast_meter_rent_12m, which(forecast_meter_rent_12m < 0), NA))


# Add constant 1 to the variables and then take log
df <- df %>% mutate( ln_forecast_cons_12m = log(forecast_cons_12m + 1),
                     ln_forecast_meter_rent_12m = log(forecast_meter_rent_12m + 1)) 

# Fill missing values with mean
df <- data.frame(sapply(df, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
# backup2 <- df

# Now check for the distribution of the transformed variables
transformed_eda <- df %>%
  select(ln_cons_12m,ln_cons_gas_12m,ln_cons_last_month,ln_imp_cons,ln_forecast_cons_12m,ln_forecast_meter_rent_12m) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = color) + theme_bw() +labs(y = NULL, x = NULL)

# Remove original columns which have been transformed
df <- df %>% select(-c(cons_12m,cons_gas_12m,cons_last_month,imp_cons,forecast_cons_12m,forecast_meter_rent_12m))


################################## DELETE ##################################

# remove_outliers(database$forecast_price_energy_p1)
boxplot(df$forecast_price_energy_p1)
df$forecast_price_energy_p1[is.na(df$forecast_price_energy_p1)]<- mean(df$forecast_price_energy_p1,na.rm=TRUE)

# remove_outliers(df$forecast_price_energy_p2)
boxplot(df$forecast_price_energy_p2)
df$forecast_price_energy_p2[is.na(df$forecast_price_energy_p2)]<-mean(df$forecast_price_energy_p2,na.rm=TRUE)

# remove_outliers(df$forecast_price_pow_p1)
boxplot(df$forecast_price_pow_p1)
df$forecast_price_pow_p1[is.na(df$forecast_price_pow_p1)]<-mean(df$forecast_price_pow_p1,na.rm=TRUE)

# remove_outliers(df$ln_forecast_cons_12m)
boxplot(df$ln_forecast_cons_12m)
df$ln_forecast_cons_12m[is.na(df$ln_forecast_cons_12m)]<-mean(df$ln_forecast_cons_12m,na.rm=TRUE)
df$ln_forecast_cons_12m[is.nan(df$ln_forecast_cons_12m)]<-mean(df$ln_forecast_cons_12m,na.rm=TRUE)

# remove_outliers(df$ln_forecast_meter_rent_12m)
boxplot(df$ln_forecast_meter_rent_12m)
df$ln_forecast_meter_rent_12m[is.na(df$ln_forecast_meter_rent_12m)]<-mean(df$ln_forecast_meter_rent_12m,na.rm=TRUE)

# remove_outliers(df$pow_max)
boxplot(df$pow_max)
df$pow_max[is.na(df$pow_max)]<-mean(df$pow_max,na.rm=TRUE)

# remove_outliers(df$ln_cons_12m)
boxplot(df$ln_cons_12m)
# df$ln_cons_12m[is.na(df$ln_cons_12m)]<-mean(df$ln_cons_12m,na.rm=TRUE)
df$ln_cons_12m[is.nan(df$ln_cons_12m)]<-mean(df$ln_cons_12m,na.rm=TRUE)

# remove_outliers(df$ln_cons_gas_12m)
boxplot(df$ln_cons_gas_12m)
#df$ln_cons_gas_12m[is.na(df$ln_cons_gas_12m)]<-mean(df$ln_cons_gas_12m,na.rm=TRUE)

# remove_outliers(df$ln_cons_last_month)
boxplot(df$ln_cons_last_month)
#df$ln_cons_last_month[is.na(df$ln_cons_last_month)]<-mean(df$ln_cons_last_month,na.rm=TRUE)
#df$ln_cons_last_month[is.nan(df$ln_cons_last_month)]<-mean(df$ln_cons_last_month,na.rm=TRUE)

# remove_outliers(df$ln_imp_cons)
boxplot(df$ln_imp_cons)
#df$ln_imp_cons[is.na(df$ln_imp_cons)]<-mean(df$ln_imp_cons,na.rm=TRUE)
# df$ln_imp_cons[is.nan(df$ln_imp_cons)]<-mean(df$ln_imp_cons,na.rm=TRUE)

# remove_outliers(df$margin_gross_pow_ele)
boxplot(df$margin_gross_pow_ele)
# df$margin_gross_pow_ele[is.na(df$margin_gross_pow_ele)]<-mean(df$margin_gross_pow_ele,na.rm=TRUE)

# remove_outliers(df$margin_net_pow_ele)
boxplot(df$margin_net_pow_ele)
# df$margin_net_pow_ele[is.na(df$margin_net_pow_ele)]<-mean(df$margin_net_pow_ele,na.rm=TRUE)

# remove_outliers(df$net_margin)
boxplot(df$net_margin)

########################################## DELETE #############################################
cor_df <- df
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

##### Correlation Matrix for df dataframe #####
cor1 <- cor(df, use = "pairwise.complete.obs")
cor_matrix <- ggcorrplot::ggcorrplot(cor1, method = "square",lab = TRUE, type = "lower", lab_size = 2.5, digits = 2,ggtheme = theme_bw)

# From the correlation matrix, we can see that 'contract_duration'& 'month_activ','num_years_antig' & 'months_end', 'margin_gross_power_ele' & 'margin_net_power_ele' have the highest correlation
# Calculate Variance Inflation Factor using the 'car' package
lm_model <- lm(churn ~ ., data = df)
vif <- data.frame(car::vif(lm_model))
vif <- rename(vif, VIF = car..vif.lm_model.)

# From the VIF we can confirm that the correlations are very high and we can drop one of the variables from each correlation pair.
df <- subset(df, select = -c(contract_duration,num_years_antig, margin_gross_pow_ele))
df_xgb <- subset(df_xgb, select = -c(contract_duration,num_years_antig, margin_gross_pow_ele))

##########################################
#             Machine Learning           #
##########################################

##### SPLIT DATA INTO TEST/TRAIN DATASET #####

intrain<- createDataPartition(df$churn,p=0.75,list=FALSE)
set.seed(1111)
train <- df[intrain,]
test <- df[-intrain,]

#########################################
##### LOGISTIC REGRESSION VIA GLM()  ####
#########################################

glm_model <- glm(churn ~ ., data = df, family = binomial("logit"))
summary(glm_model)

# df_test <- subset(df, select = c(forecast_discount_energy,forecast_price_energy_p1,forecast_price_energy_p2,forecast_price_pow_p1,margin_gross_pow_ele,margin_net_pow_ele,nb_prod_act,net_margin,num_years_antig,pow_max,churn,contract_duration,months_active,months_end,months_modif,months_renewal,channel_epum,channel_ewpa,channel_fixd,channel_foos,channel_lmke,channel_sddi,channel_usil,has_gas_1,ln_cons_12m,ln_cons_gas_12m,ln_cons_last_month,ln_imp_cons,ln_forecast_cons_12m,ln_forecast_meter_rent_12m))
# glm_model <- lm(churn~., data = df, family = binomial("logit"),control=glm.control(maxit=100))

##########################################
##### LOGISTIC REGRESSION VIA CARET  #####
##########################################

# Logistic Regression via Caret Package

# define training control
train_control <- trainControl(method = "cv", number = 10)

# ------------Model1-----------------

# train the model on training set
glm_model1 <- train(as.factor(churn)~.,
                         data = train,
                         trControl = train_control,
                         method = "glm",
                         family=binomial(link = "logit"))

print(glm_model1)
summary(glm_model1)
varImp(glm_model1, scale = FALSE)

?varImp

# ------------Model2------------------
glm_model2 <- train(as.factor(churn)~. -channel_sddi -channel_fixd -channel_epum -net_margin -ln_cons_gas_12m -ln_forecast_meter_rent_12m -nb_prod_act -forecast_price_pow_p1 -months_end,
                                  data = df,
                                  trControl = train_control,
                                  method = "glm",
                                  na.action = na.pass,
                                  family=binomial(link = "logit"))

print(glm_model2) 
summary(glm_model2) 
  
# ------------Model3------------------  

glm_model3 <- train(as.factor(churn)~. -channel_sddi -channel_fixd -channel_epum -net_margin -ln_cons_gas_12m -ln_forecast_meter_rent_12m -nb_prod_act -forecast_price_pow_p1 -months_end -channel_usil -months_modif -forecast_discount_energy -ln_forecast_cons_12m -pow_max,
                                  data = df,
                                  trControl = train_control,
                                  method = "glm",
                                  na.action = na.pass,
                                  family=binomial(link = "logit"))

print(glm_model3)
summary(glm_model3) 

# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = glm_model1, 
      model2 = glm_model2, 
      model3 = glm_model3
    )
  )
)$statistics$Accuracy # We use model 3 as it has a slight higher accuracy and uses lesser coefficients

#Caret Model  Prediction
pred_class_test <- predict(glm_model1, newdata = test)
pred_type_test <- predict(glm_model1, newdata = test, type = "prob")
count(pred_class_test)

cm_glm <- confusionMatrix(as.factor(pred_class_test),as.factor(test$churn))

pred_type_test <- rename(pred_type_test, 'churned' = '1' )
pred_type_test <- rename(pred_type_test, 'retained' = '0' )

# Producing ROC curve of model
pred_response <- predict(glm_model1, newdata = test, type = "response")
predictFull <- prediction(pred_type_test$churned,as.factor(test$churn))

# Plot AUC
auc_roc <- performance(predictFull, measure = 'tpr',x.measure = 'fpr')
plot(auc_roc, col = "blue")

auc_score_glm <- auc(test$churn, pred_type_test$churned) #0.6221063
rmse_score_glm <- rmse(test$churn, pred_type_test$churned) # 0.2994171


######################################################################

#########################
##### XGBoost Model #####
#########################


# df_xgb_backup<- df_xgb
# df_xgb <- df_xgb_backup

intrain<- createDataPartition(df_xgb$churn,p=0.75,list=FALSE)
set.seed(2018)
ml_train <- df[intrain,]
ml_test <- df[-intrain,]

labels <- ml_train$churn
ts_label <- ml_test$churn

#XGBoost takes matrix for data hence we convert dataframe to matrix
# df_xgb_backup<- df_xgb
xgb_train <- ml_train %>% select(-churn)
xgb_train <- xgb.DMatrix(data = as.matrix(xgb_train),label = labels)

xgb_test <- ml_test %>% select(-churn)
xgb_test <- xgb.DMatrix(data = as.matrix(xgb_test),label = ts_label)

#default parameters
xgb_params <- list(booster = "gbtree", 
                   objective = "binary:logistic", 
                   eta=0.3, gamma=0, 
                   max_depth=6, 
                   min_child_weight=1, 
                   subsample=1, 
                   colsample_bytree=1)

# Calculate the best nround for this model. In addition, this function also returns CV error, which is an estimate of test error.
xgbcv <- xgb.cv( params = xgb_params, data = xgb_train, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F, missing = NA)


elog <- as.data.frame(xgbcv$evaluation_log)
nround <- which.min(elog$test_error_mean)
##best iteration = 43
## The model returned lowest error at the 21st (nround) iteration.
# CV accuracy is 1-0.0941 = 90.6%


xgb_model <- xgb.train(params = xgb_params, 
                       data = xgb_train)
#first default - model training
xgb_model <- xgb.train (params = xgb_params, 
                        data = xgb_train, 
                        nrounds = nround, 
                        watchlist = list(train=xgb_train,test=xgb_test), 
                        print_every_n = 10, early_stop_round = 10, 
                        maximize = F , 
                        eval_metric = c("error","RMSE"))

#model prediction
xgbpred <- predict(xgb_model,xgb_test)
xgbpred <- ifelse(xgbpred > 0.5,1,0)


# The objective function binary:logistic returns output probabilities rather than labels. To convert it, we need to 
# manually use a cutoff value. As seen above, I've used 0.5 as my cutoff value for predictions. We can calculate our model's
# accuracy using confusionMatrix() function from caret package.

#confusion matrix
cm_xgb <- confusionMatrix (as.factor(xgbpred), as.factor(ts_label))
#Accuracy - 86.54%` 

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(df_xgb),model = xgb_model)

# The ggplot-backend method also performs 1-D clustering of the importance values, with bar colors 
# corresponding to different clusters that have somewhat similar importance values.
xgb_feature_plot <- xgb.ggplot.importance (importance_matrix = mat)

# This tells you that satisfaction level is the most important variable across all predictions, but there's no guarantee it's 
# the most important for this particular employee. Also, good luck trying to explain what the x-axis means to your senior 
# stakeholder. It is the Gain contribution of each feature to the model, where Gain is defined as:

###############################
##### Random Forest Model #####
###############################

# RandomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
# Arguments:
# - Formula: Formula of the fitted model
# - ntree: number of trees in the forest
# - mtry: Number of candidates draw to feed the algorithm. By default, it is the square of the number of columns.
# - maxnodes: Set the maximum amount of terminal nodes in the forest
# - importance=TRUE: Whether independent variables importance in the random forest be assessed

# Using the common ml split data with missing values as Random Forest can cater to missing values

rf_train <- ml_train
rf_test <- ml_test 

# Set K-fold cross validation settings and controls
trControl <- trainControl(method = "cv",    # The method used to resample the datasets  
                          number =  10,      # Number of folds to create
                          search = "grid")  # Use the search grid method

set.seed(1234)
# Run the model with default parameters
rf_model_caret <- train(as.factor(churn)~.,
                    data = rf_train,
                    method = "rf",
                    metric = "accuracy",
                    trControl = trControl,
                    )

?train

# Print the results
print(rf_model_caret)

plot(rf_model_caret)

# The algorithm uses 500 trees and tested three different values of mtry: 2, 14, 26.
# The final value used for the model was mtry = 14 with an accuracy of 0.904. Let's try to get a higher score.

# Find the best number of mtry
# We can test the model with values of mtry from 1 to 10

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(as.factor(churn)~.,
                 data = rf_train,
                 method = "rf",
                 metric = "accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 5)

print(rf_mtry)

max(rf_mtry$results$Accuracy)

best_mtry <- rf_mtry$bestTune$mtry 

# Maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(as.factor(churn)~.,
                      data = rf_train,
                      method = "rf",
                      metric = "oob",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 5)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)


# Train a Random Forest
rf_model <- randomForest(as.factor(churn)~., data = rf_train)

# Grab OOB error matrix & take a look
err <- rf_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err) #0.09244533 

# Plot the model trained
rf_error_model <- plot(rf_model)
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))

# Generate predicted classes using the rf_model object with type = "class"
class_prediction <- predict(object = rf_model,  # model object 
                            newdata = rf_test,  # test dataset
                            type = "class")         # return classification labels

# Calculate the confusion matrix for the test set
cm_class <- confusionMatrix(data = as.factor(class_prediction),          # predicted classes
                      reference = as.factor(rf_test$churn))  # actual classes

summary(class_prediction)
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

# Generate predictions on the test set with type = "prob"
type_prediction <- predict(object = rf_model, 
                newdata = rf_test,
                type = "prob")

# Look at the pred format
head(type_prediction)           

cm_prob <- confusionMatrix(data = as.factor(type_prediction),as.factor(rf_test$churn))

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc(actual = rf_test$churn, 
    predicted = type_prediction[,"0"])   


ggplot(aes(x=churn), data=df_draft) +
  geom_histogram(fill='dark orange')

?randomForest


###############################
#####  Residual Analysis  #####
###############################

# Get the predicted y values from the model
df$reg_linear_y_pred <- reg_linear$fitted.values
# Calculate the errors of the model
df$reg_linear_res <- df$ln_deaths_per_capita - df$reg_linear_y_pred

# Find countries with largest negative errors
df %>% top_n( -5 , reg_linear_res ) %>% 
  select( country ,death, ln_deaths_per_capita , reg_linear_y_pred, reg_linear_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg_linear_res ) %>% 
  select( country, death,  ln_deaths_per_capita , reg_linear_y_pred, reg_linear_res )


##################################
##### Prediction uncertainty #####
##################################

# CI of predicted value/regression line is implemented in ggplot
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' , se = T )

##
# You can get them by predict function
#   interval can be any of c("none", "confidence", "prediction")
#   alpha = 0.05 (default) is the significance level
###
# CI of regression line
pred4_CI <- predict( reg4, newdata = df , interval ="confidence" , alpha = 0.05 )
pred4_CI

# If you want you can ask to calculate the SEs for each point:
# pred4_CI <- predict( reg4, newdata = df , se.fit=T,
#                  interval ="confidence" , alpha = 0.05 )

# Hand made CI for regression line
# 1) Add to datatset:
df <- df %>% mutate( CI_reg4_lower = pred4_CI$fit[,2],
                     CI_reg4_upper = pred4_CI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_gdppc, y = lifeexp ) , color='blue') +
  geom_line( data = df, aes( x = ln_gdppc, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_gdppc, y = CI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = df, aes( x = ln_gdppc, y = CI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "ln( GDP/capita, 2018 int. const. $, PPP)",y = "Life expectancy  (years)") 


##
# Now we change to get the prediction intervals!
#
pred4_PI <- predict( reg4, newdata = df , interval ="prediction" , alpha = 0.05 )

# Hand made Prediction Interval for regression line
# 1) Add to datatset (You can use the SE's as well if you wish...
#                        then alpha does not have any meaning)
df <- df %>% mutate( PI_reg4_lower = pred4_PI$fit[,2],
                     PI_reg4_upper = pred4_PI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_gdppc, y = lifeexp ) , color='blue') +
  geom_line( data = df, aes( x = ln_gdppc, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_gdppc, y = PI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dotted" ) +
  geom_line( data = df, aes( x = ln_gdppc, y = PI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dotted" ) +
  labs(x = "ln( GDP/capita, 2018 int. const. $, PPP)",y = "Life expectancy  (years)") 





test_count <- sapply(train, function(y) sum(length(which(is.na(y)))))
test_count<- data.frame(test_count)

df %>% filter(is.na())

