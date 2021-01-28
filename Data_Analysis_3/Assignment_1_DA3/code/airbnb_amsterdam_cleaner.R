
library(tidyverse)
library(data.table)
library(stringr)  

# Fix link
listings = read.csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_1_DA3/data/listing.csv")
data <- listings
glimpse(data)
###################
#  Data Cleaning  #
###################

# Filter for apartments which accomodate 2-6 persons
properties_type <- as.data.frame(table(data$property_type))

data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire serviced apartment")) %>%
  filter(between(accommodates, 2, 6))

# Drop unnecessary columns
data <- data[grep("^host", colnames(data), invert = TRUE)]
data <- data[grep("^calculated", colnames(data), invert = TRUE)]
data <- data %>% select(-contains("maximum"))
data <- data %>% select(-c("listing_url","scrape_id","last_scraped","name","description","neighborhood_overview","picture_url",
                           "neighbourhood_group_cleansed","bathrooms","minimum_minimum_nights", "minimum_nights_avg_ntm","calendar_updated",
                           "calendar_last_scraped","number_of_reviews_ltm","number_of_reviews_l30d","license","reviews_per_month",
                           "availability_30","availability_60","availability_90","availability_365","neighbourhood","has_availability"))

#amenities
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))
#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <- cbind(data, as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), 
                                                        table))))

data <- data %>% select(-(194:243))
# names(data) <- str_trim(names(data), "left")
names(data) <- str_replace_all(names(data), " ", "_")
names(data) <- gsub('^_','',names(data))

# data <- data %>% select(-contains(c("]","[")))

#Checkpoint 1: Initial column cleaning
backup_a <- data
data <- backup_a


# data <- data %>% select(-c("Amazon Video Prime", "Mini fridge", "and wardrobe","Changing table","Children\u2019s books and toys",
# "Children\u2019s dinnerware","Complimentary self parking", "Free driveway parking on premises \u2013 1 space",
# "Free parking on premises \u2013 1 space", "Free residential garage on premises", "Radiant heating",
# "Valet parking \u2014 \u20ac35/day","Babysitter recommendations","-`  bluetooth connection for you to connect your Spotify sound system with Bluetooth and aux`",
# "Luggage store possible ( small fee)// * I wash your dishes. Enjoy holiday!//* My fridge in kitchen","L'Oreal","portable"))

### Updated aggregate_columns function code ###
# Example: Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

# rename some columns for easier aggregation
names(data)[names(data) == "Mini_fridge"] <- "Mini_frige"
names(data)[names(data) == "Nespresso_machine"] <- "Nespresso_coffee_machine"
names(data)[names(data) == "Shower_gel"] <- "Shower_gel_soap"
names(data)[names(data) == "Barbecue_utensils"] <- "BBQ_utensils"
names(data)[names(data) == "Freezer"] <- "Freezer_frige"
names(data)[names(data) == "Free_residential_garage_on_premises`"] <- "free_garage_parking"


amenities_clean_df <- sapply(data[25:193], function(x){sum(x)})
amenities_clean_df <- data.frame(amenities_clean_df)

# Pass a vector of phrases to the for loop to make the process quicker
column_names <- c("stove","Wifi","TV","oven","frige", "soap", "BBQ", "toys", "crib", "parking", "shampoo", "heating","washer","toiletries","conditioner","dry")

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

# contains_list <- c("breakfast")
# data <- data %>% select(-contains(contains_list))

# Checkpoint 2:Subset data further for cleaning
backup_b <- data
data <- backup_b

count_df <- sapply(data[25:131], function(x){sum(x)})
count_df <- data.frame(count_df)

# Subset all ameneties columns and remove any which have '1' less than 5%
amenities_clean <- data %>% select(25:131, "id")
less_than_5per <- amenities_clean %>% select(where(~mean(. == 1) <= 0.005))
less_than_5per <- less_than_5per %>% select(-contains(c("id")))
amenities_clean <- amenities_clean %>% select(-colnames(less_than_5per))

# Check for count
amenities_clean_df <- sapply(amenities_clean, function(x){sum(x)})
amenities_clean_df <- data.frame(amenities_clean_df)

# Merge the original and amenities dataframe
data <- data %>% select(-(25:131))
data <- merge(data,amenities_clean, by = "id", all = FALSE)

#remove dollar signs from price variable. These prices are actually Euros
data$price<-gsub("\\$","",as.character(data$price))
data$price<-as.numeric(as.character(data$price))

data <- data %>% select(-c("amenities", "Babysitter_recommendations", "Baby_bath", "Baking_sheet"))

names(data)[names(data) == "frige_agg"] <- "refrigerator"

data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_1_DA3/data/"
write.csv(data,file=paste0(data_out,"amsterdam_clean_prep.csv"), row.names = FALSE)
