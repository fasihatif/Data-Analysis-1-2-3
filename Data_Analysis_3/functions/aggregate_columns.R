aggregate_columns <- function(word){
  
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
  
  # Convert new column from character to integer
  data$col_name <- as.integer(data$col_name)
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  data <<- data %>% select(-colnames(new_df))
}

#### Use Case ####
# Aggregates all 'stove' related columns into one generic binary stove column.
# Make sure to rename the new binary column everytime you run the function.

# aggregate_columns("stove")
# data <- data %>% rename("stove_gas_or_elect" = col_name)



