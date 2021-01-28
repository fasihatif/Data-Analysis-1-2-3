
# Fix link
data = read.csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_1_DA3/data/listings.csv")
data <- listings

library(tidyverse)
library(data.table)


###################
#  Data Cleaning  #
###################

# Drop unnecessary columns

data <- data[grep("^host", colnames(data), invert = TRUE)]
data <- data[grep("^calculated", colnames(data), invert = TRUE)]
data <- data %>% select(-contains("maximum"))
data <- data %>% select(-c("listing_url","scrape_id","last_scraped","name","description","neighborhood_overview","picture_url",
                           "neighbourhood_group_cleansed","bathrooms","minimum_minimum_nights", "minimum_nights_avg_ntm","calendar_updated",
                           "calendar_last_scraped","number_of_reviews_ltm","number_of_reviews_l30d","license","reviews_per_month",
                           "availability_30","availability_60","availability_90","availability_365","neighbourhood","has_availability"))

data <- data %>% select(-`  bluetooth connection for you to connect your Spotify sound system with Bluetooth and aux`)

#remove dollar signs from price variable
data$price<-gsub("\\$","",as.character(data$price))
data$price<-as.numeric(as.character(data$price))


#amenities
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-as.list(strsplit(data$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data<-cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))

# data <- data %>% select(-contains(c("]","[")))

#Checkpoint
aggregate_backup <- data
data <- aggregate_backup

### Updated aggregate_columns function code ###
# Example: Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

# Pass a vector of phrases to make the process quicker
column_names <- c("stove","Wifi","TV","oven","frige","Paid parking", "soap","shampoo","Self-parking","washer","toiletries","conditioner")

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
  data$col_name <- as.integer(data$col_name)
  names(data)[names(data) == 'col_name'] <- paste0(word,"_agg")
  
}

# Function version
aggregate_columns("sound")
data <- data %>% rename("sound_system" = col_name)
data$sound_system <- as.integer(data$sound_system)

contains_list <- c("breakfast", "L'Oreal","portable")
data <- data %>% select(-contains(contains_list))

# Experimenting
count_df <- sapply(data[75:227], function(x){sum(x)})
count_df <- data.frame(count_df)

data <- data %>% data[, -which(numcolwise(sum(data) < 10))]


# checkpoint
df_backup <- data
data <- df_backup

### Updated aggregate_columns function code ###
# Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

aggregate_columns("sound")
data <- data %>% rename("sound_system" = col_name)
data$sound_system <- as.integer(data$sound_system)




data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_1_DA3/data/"
write.csv(data,file=paste0(data_out,"amsterdam_amenties.csv"), row.names = FALSE)
