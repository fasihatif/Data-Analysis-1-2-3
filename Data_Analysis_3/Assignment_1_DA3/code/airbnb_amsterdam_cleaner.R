

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


# Function which merges several columns of same type into one generic binary column

dummy_category <- function(word,df_name,df_name_merge){
  df_name <- data %>% select(contains(word),"id")
  df_name$col_name <- apply(df_name[0:ncol(df_name)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  df_name_merge <- df_name %>% select(id,col_name)
  data <- merge(data,df_name_merge,by = "id", all = FALSE)
  df_name <- df_name %>% select(-c(id,col_name))
  data <<- data %>% select(-colnames(df_name))
}

# checkpoint
df_backup <- data
data <- df_backup

# Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

dummy_category("sound",sound_system, sound_df,sound_df_merge)
data <- data %>% rename("sound_system" = col_name)

dummy_category("stove",stove_df,stove_df_merge)
data <- data %>% rename("stove_gas_or_elect" = col_name)

dummy_category("Wifi",wifi_df,wifi_df_merge)
data <- data %>% rename("wifi" = col_name)

dummy_category("TV",TV_df,TV_df_merge)
data <- data %>% rename("tv" = col_name)

dummy_category("oven",oven_df,oven_df_merge)
data <- data %>% rename("oven" = col_name)

dummy_category("refrigerator",refrigerator_df,refrigerator_df_merge)
data <- data %>% rename("refrigerator" = col_name)

dummy_category("Paid parking",paid_park_df,paid_park_df_merge)
data <- data %>% rename("paid_parking" = col_name)

data$`  bluetooth connection for you to connect your Spotify sound system with Bluetooth and aux`
drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
data<-data[ , !(names(data) %in% drops)]

data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_1_DA3/data/"
write.csv(data,file=paste0(data_out,"amsterdam_amenties.csv"), row.names = FALSE)
