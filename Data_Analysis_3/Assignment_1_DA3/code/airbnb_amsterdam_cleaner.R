

data = read.csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_1_DA3/data/listings.csv")
data <- listings


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
data <- data %>% select(-contains("conditioner"))
data <- data %>% select(-contains("soap"))
data <- data %>% select(-contains("portable"))
data <- data %>% select(-contains("Self-parking"))
data <- data %>% select(-contains("shampoo"))

#remove dollar signs from price variable
data$price<-gsub("\\$","",as.character(data$price))
data$price<-as.numeric(as.character(data$price))


#amenities
data$amenities<-gsub("\\{","",data$amenities)
data$amenities<-gsub("\\}","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-as.list(strsplit(data$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data<-cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))

data <- data %>% select(-contains(c("]","[")))


# Function which aggregates several columns of same type/category into one generic binary column. For example, several TV columns into one generic TV column

aggregate_columns <- function(word){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- data %>% select(contains(word),"id")
  
  #Go row by row to see if any of the rows have a 1. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  data <<- data %>% select(-colnames(new_df))
}

# checkpoint
df_backup <- data
data <- df_backup

### Updated aggregate_columns code
# Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

aggregate_columns("sound")
data <- data %>% rename("sound_system" = col_name)

aggregate_columns("stove")
data <- data %>% rename("stove_gas_or_elect" = col_name)

aggregate_columns("Wifi")
data <- data %>% rename("wifi" = col_name)

aggregate_columns("TV")
data <- data %>% rename("tv" = col_name)

aggregate_columns("oven")
data <- data %>% rename("oven" = col_name)

aggregate_columns("refrigerator")
data <- data %>% rename("refrigerator" = col_name)

aggregate_columns("Paid parking")
data <- data %>% rename("paid_parking" = col_name)

data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_1_DA3/data/"
write.csv(data,file=paste0(data_out,"amsterdam_amenties.csv"), row.names = FALSE)
