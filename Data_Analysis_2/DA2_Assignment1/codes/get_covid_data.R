# Clear memory and call packages
rm(list=ls())
library(WDI)
library(tidyverse)

# Download COVID cross-sectional data
covid_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-24-2020.csv'
my_path <- 'C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_2/DA2_Assignment1/data/'

download.file(covid_url, paste0(my_path,'raw/covid_raw.csv'))
covid_raw <- read.csv(covid_url)

# Download population data for 2019
pop_raw <- WDI(indicator=c('SP.POP.TOTL'), 
               country="all", start=2019, end=2019)

write_csv(pop_raw, paste0(my_path,'raw/pop_WDI_2019.csv'))