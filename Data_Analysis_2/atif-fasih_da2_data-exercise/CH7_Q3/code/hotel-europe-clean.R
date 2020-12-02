#############################################
#
# hotels-eruope cleaning
# input:
# hotelbookingdata.csv

# output:
# hotel-europe_prices.csv
# hotel-europe_features.csv

#############################################

dir <- 'C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_2/atif-fasih_da2_data-exercise/CH7_Q3/data'

# PACKAGES
library(dplyr)
library(tidyverse)

### IMPORT AND PREPARE DATA

# variables downoaded as string, often in form that is not helpful
# need to transform then to numbers that we can use

df <- read_csv(paste0(dir,"/raw/hotelbookingdata.csv"))

# generate numerical variable of rating variable from string variable
#  trick: remove non-numeric characters using regex

# distance to center entered as string in miles with one decimal
df$distance <- as.numeric(gsub("[^0-9\\.]","",df$center1distance))
df$distance_alter <- as.numeric(gsub("[^0-9\\.]","",df$center2distance))

# parsing accommodationtype column
# replace missing values to handle split
df[df$accommodationtype == "_ACCOM_TYPE@",]$accommodationtype <- "_ACCOM_TYPE@NA"
df$accommodation_type <- unlist(sapply(strsplit(as.character(df$accommodationtype), "@"), '[[', 2))
df$accommodationtype <- NULL

# number of nights variable
df$nnights <- 1
df[df$price_night == "price for 4 nights",]$nnights <- 4

# ratings
# generate numerical variable of rating variable from string variable
# remove /5

df$rating <- as.numeric(gsub("/5","",df$guestreviewsrating))

# check: frequency table of all values
table(df$rating)

# RENAME VARIABLES
#colnames(df)[colnames(df)=="rating_reviewcount"] <- "rating_count"
colnames(df)[colnames(df)=="rating2_ta"] <- "ratingta"
colnames(df)[colnames(df)=="rating2_ta_reviewcount"] <- "ratingta_count"
colnames(df)[colnames(df)=="addresscountryname"] <- "country"
colnames(df)[colnames(df)=="s_city"] <- "city"

# look at key vars
colnames(df)[colnames(df)=="starrating"] <- "stars"
table(df$stars)
df$stars[df$stars == 0] <- NA

table(df$rating)

# drop if hotel id is missing
df <- df[!is.na(df$hotel_id), ]

# drop vars
df$center2distance <-  NULL
df$center1distance <-  NULL
df$price_night <- NULL
df$guestreviewsrating <- NULL

# DROP PERFECT DUPLICATES
df[duplicated(df)==T,]
#these are perfect duplicates of the observation in the previous row
df <- df[!duplicated(df), ]


# drop if the row is the same based on the most important variables
df <- df[!duplicated(subset(df, select = c(city, hotel_id, distance, stars, rating, price, year, month, weekend, holiday))), ]

write_csv(df, paste0(dir,"/clean/hotels-europe_clean.csv"))
