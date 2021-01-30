
library(tidyverse)
library(data.table)
library(stringr)  
library(fastDummies)
library(ggpubr)
library(scales)

#------------------------------------------------------------------------------

# Import data

listings = read.csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_1_DA3/data/listing.csv")
data <- listings
glimpse(data)

#########################
## FEATURE ENGINEERING ##
#########################

# ---------------------------- DATA CLEANING ----------------------------------#

table(data$property_type)

data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire serviced apartment", "Entire home/apt", "Private room in apartment", "Private room in serviced apartment", "Shared room in apartment", "Room in serviced apartment")) %>%
  filter(between(accommodates, 2, 6))

# Drop unnecessary columns
data <- data[grep("^host", colnames(data), invert = TRUE)]
data <- data[grep("^calculated", colnames(data), invert = TRUE)]
data <- data %>% select(-contains("maximum"))
data <- data %>% select(-c("listing_url","scrape_id","last_scraped","name","description","neighborhood_overview","picture_url",
                           "neighbourhood_group_cleansed","bathrooms","minimum_minimum_nights", "minimum_nights_avg_ntm","calendar_updated",
                           "calendar_last_scraped","number_of_reviews_ltm","number_of_reviews_l30d","license","reviews_per_month",
                           "availability_30","availability_60","availability_90","availability_365","neighbourhood","has_availability"))

# Format amenities column. Remove square brackets and convert to vector
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))
#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <- cbind(data, as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), 
                                                        table))))

data <- data %>% select(-(224:273))

# Remove all whitespaces from column names
names(data) <- trimws(names(data))

# Repace all spaces between words with underscores
names(data) <- str_replace_all(names(data), " ", "_")

#Checkpoint 1: Initial column cleaning
backup_a <- data
data <- backup_a

#-------------------------------------------------------------------------------

# rename some columns for easier aggregation
names(data)[names(data) == "Mini_fridge"] <- "Mini_frige"
names(data)[names(data) == "Shower_gel"] <- "Shower_gel_soap"
names(data)[names(data) == "Barbecue_utensils"] <- "BBQ_utensils"
names(data)[names(data) == "Freezer"] <- "Freezer_frige"
names(data)[names(data) == "Free_residential_garage_on_premises"] <- "free_garage_parking"
names(data)[names(data) == "Amazon_Prime_Video"] <- "Amazon_Prime_TV"


# To eyeball the column names
amenities_clean_df <- sapply(data[25:193], function(x){sum(x)})
amenities_clean_df <- data.frame(amenities_clean_df)

# ------------------------------------------------------------------------------

### Updated aggregate_columns function code ###
# Example: Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

# Pass a vector of phrases to the for loop to make the process quicker
column_names <- c("sound", "stove","Wifi","TV","oven","frige", "soap", "BBQ", "toys", "crib", "parking", "shampoo", "heating","washer","toiletries","conditioner","dry")

for( word in column_names){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- data %>% select(contains(word),"id")
  
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  data <- data %>% select(-colnames(new_df))
  
  # Convert from character to integer
  data$col_name <- as.integer(data$col_name)
  
  # Rename new column
  names(data)[names(data) == 'col_name'] <- paste0(word,"_agg")
  
} 

#-------------------------------------------------------------------------------
# contains_list <- c("breakfast")
# data <- data %>% select(-contains(contains_list))

# Checkpoint 2:Subset data further for cleaning
backup_b <- data
data <- backup_b

count_df <- sapply(data[25:125], function(x){sum(x)})
count_df <- data.frame(count_df)

# Subset all ameneties columns and remove any which have '1' less than 5%
amenities_clean <- data %>% select(25:125, "id")
less_than_5per <- amenities_clean %>% select(where(~mean(. == 1) <= 0.005))
less_than_5per <- less_than_5per %>% select(-contains(c("id")))
amenities_clean <- amenities_clean %>% select(-colnames(less_than_5per))

# Check for count
amenities_clean_df <- as.data.frame(sapply(amenities_clean, function(x){sum(x)}))

# Merge the original and amenities dataframe
data <- data %>% select(-(25:125))
data <- merge(data,amenities_clean, by = "id", all = FALSE)

#remove dollar signs from price variable. These prices are actually Euros
data$price<-gsub("\\$","",as.character(data$price))
data$price<-as.numeric(as.character(data$price))

data <- data %>% select(-c("amenities", "Babysitter_recommendations", "Baby_bath", "Baking_sheet"))

names(data)[names(data) == "frige_agg"] <- "refrigerator"
names(data)[names(data) == "neighbourhood_cleansed"] <- "neighbourhood"
names(data)[names(data) == "bathrooms_text"] <- "bathrooms"

# Remove text from bathrooms column
table(data$bathrooms)

data$bathrooms <- replace(data$bathrooms,data$bathrooms == '1 bath',1)
data$bathrooms <- replace(data$bathrooms,data$bathrooms == 'Half-bath',0.5)
data$bathrooms <- gsub("baths", "", data$bathrooms)
data$bathrooms <- as.numeric(data$bathrooms)

#-------------------------------------------------------------------------------

backup_c <- data
data <- backup_c

# ----------------------------------------------------------------
## Create dummy variables using the fastdummies library
data <- data %>% dummy_cols(select_columns = "instant_bookable", remove_selected_columns = TRUE)
data <- data  %>% select(-c("instant_bookable_f"))

# create dummy vars
dummies <- names(data)[seq(24,89)]

data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))

dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# ------------------------------------------------------------------------------

# Convert room type to factor
table(data$room_type)

# Rename room type
data$room_type <- replace(data$room_type,data$room_type == 'Entire home/apt', "Entire_apt")
data$room_type <- replace(data$room_type,data$room_type == 'Hotel room', "Private room")
data$room_type <- replace(data$room_type,data$room_type == 'Private room', "Private_room")
data$room_type <- replace(data$room_type,data$room_type == 'Shared room', "Shared_room")

data <- data %>%
  mutate(f_room_type = factor(room_type))

# Convert neighbourhood_cleansed to factors
data <- data %>%
  mutate(
    f_neighbourhood = factor(neighbourhood))


# Property Type
table(data$property_type)

data$property_type <- replace(data$property_type,data$property_type == 'Entire apartment', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Entire home/apt', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Entire serviced apartment', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Room in serviced apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Private room in apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Private room in serviced apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Shared room in apartment', 'Room_apartment')


data <- data %>%
  mutate(f_property_type = factor(property_type))


# ------------------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds","minimum_nights", "number_of_reviews", "review_scores_rating")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#-------------------------------------------------------------------------------

# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(id,price,matches("^d_.*|^n_.*|^f_.*"))

# ---------------------------Exploratory Data Analysis ------------------------#

backup_d <- data
data <- backup_d

###### Price ######

summary(data$price)
describe(data$price)

# price boxplot
boxplot(data$price)

# Take log of price
data <- data %>%
  mutate(ln_price = log(price))


# Remove extreme values
data <- data %>%
  filter(price < 650)

# Price Distribution
price_hist <- ggplot(data, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "grey", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price")


ln_price_hist <- ggplot(data, aes( x = ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "grey", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price (log)")

price_hist_grid <- ggarrange(
  price_hist,
  ln_price_hist,
  nrow = 1
)

# ------------------------------------------------------------------------------

###### Accommodates ######

price_hist <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour= "grey", shape=16)+
 # ylim(0,800)+
# xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour= "red", se=FALSE)+
  theme_bw()

# Squares and further values to create for accommodation
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates))

###### Beds ######

## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_beds, y=price)) +
  geom_point(size=1, colour= "grey", shape=16)+
  # ylim(0,800)+
  # xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour= "red", se=FALSE)+
  theme_bw()


# Take logs of beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

###### Bathrooms ######
table(data$n_bathrooms)

ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "grey", color = "black") +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bw()

# Pool accommodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,5), labels=c(0,1,2), right = F) )

###### Reviews ######
ggplot(data, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "red", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bw()

data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bw()

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, shape=4) +
 # ylim(0,800)+
  #xlim(20,100)+
  geom_smooth(method="loess", se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bw()


# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))


# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

# ------------------------------------------------------------------------------

# Number of missing values in each column
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Price, bathrooms, review_scores_rating, n_bedrooms, n_beds columns have missing values

# Since Price has only 16 missing values, we will drop the observations with missing price values
data <- data %>% 
  drop_na(price)


# 2. impute when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
    n_bedrooms=ifelse(is.na(n_bedrooms),1, n_bedrooms)
  )

data <- data %>%
  mutate(
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating))

data <- data %>% select(-ln_review_scores_rating)

# Cleaned data
data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_1_DA3/data/"
write.csv(data,file=paste0(data_out,"amsterdam_clean.csv"), row.names = FALSE)
