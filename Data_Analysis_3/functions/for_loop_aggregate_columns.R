# Assign a vector of phrases
# Faster than the aggregate_columns functon


column_names <- c()
# column_names <- c("stove", "parking", "breakfast")


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
  
  data$col_name <- as.numeric(data$col_name)
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  data <- data %>% select(-colnames(new_df))
  
  # Rename the new column
  names(data)[names(data) == 'col_name'] <- paste0(word,"_agg")
  
}


