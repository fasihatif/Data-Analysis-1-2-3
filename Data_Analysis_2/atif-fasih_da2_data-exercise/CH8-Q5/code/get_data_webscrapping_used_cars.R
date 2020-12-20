library(rvest)
library(tidyverse)
library(data.table)

used_cars <- function(my_url){
  
  url <- read_html(my_url)
  
  model_name <- url %>%
    html_nodes('.vehicle-header-make-model') %>%
    html_text()
  if (length(model_name) ==0) {
    model_name <- ''} else {
      model_name <- model_name
    }
  
  
  make_year <- url %>%
    html_nodes('.vehicle-card-year') %>%
    html_text()
  if (length(make_year) ==0) {
    make_year <- ''} else{
      make_year <- as.numeric(make_year)
    }
  
  car_price <- url %>%
    html_nodes('.margin-y-1') %>%
    html_text()
  if (length(car_price) ==0) {
    car_price <- ''} else{
      car_price <- as.numeric(gsub('[[:punct:]]','',car_price))
    }
  
  
  
  df <- data.frame('model_name' = model_name, 'make_year' = make_year, 'car_price' = car_price)
  return(df)
  
}

car_urls <- paste0('https://www.truecar.com/used-cars-for-sale/listings/ford/fusion/?page=',1:100,'&sort[]=best_match')
df_list <- lapply(car_urls, used_cars)
database <- rbindlist(df_list)
database <- database %>% mutate('age' = 2020 - make_year)

write.csv(database,'used_cars_ford.csv')

