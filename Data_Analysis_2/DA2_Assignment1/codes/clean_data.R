# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# Read the raw files

my_path <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_2/DA2_Assignment1/data/"

### covid data ###
covid_raw <- read_csv(paste0(my_path,'raw/covid_raw.csv'))
             
# Analyze covid data       
glimpse(covid_raw)

# I will be renaming variables during cleaning process based on actions i perform as a form of checkpoint.

# Drop irrelevant columns
covid_clean_col <- covid_raw %>% select( -c(FIPS, Admin2,  Last_Update,  Lat,  Long_,Combined_Key, Incidence_Rate, 'Case-Fatality_Ratio'))
covid_clean_col

# Group countries and sum the amounts
covid_clean_grouped <- covid_clean_col %>% 
  group_by(Country_Region) %>%
  summarise_if(is.numeric,lst( sum ) )
  
# Rename columns
covid_clean_colrename <- covid_clean_grouped %>%
  rename( country   = Country_Region ,
           confirmed = Confirmed_sum,
           death     = Deaths_sum,
           recovered = Recovered_sum,
           active    = Active_sum )

# Rename variable to something short
cv <- covid_clean_colrename #### fix

# Remove extra variables
rm(covid_clean_col,covid_clean_colrename, covid_clean_grouped)


### population data ###

# population data
pop_raw <- read_csv(paste0(my_path, 'raw/pop_WDI_2019.csv'))
                    
# Remove federations, groups, and organizations from the country column

pop_raw_country_filter<- pop_raw %>% filter(!grepl("[[:digit:]]",pop_raw$iso2c))

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 2nd drop specific values
drop_id <- c("EU","HK","OE")
pop_raw_country_filter <- pop_raw_country_filter %>% filter( !grepl( paste( drop_id , collapse="|"), pop_raw_country_filter$iso2c)) 

# Get the first letter from iso2c
fl_iso2c <- substr(pop_raw_country_filter$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")
# Filter out everything which starts X or Z except countries in retain_id
pop_raw_country_filter <- pop_raw_country_filter %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                            !grepl( paste( retain_id , collapse="|"), pop_raw_country_filter$iso2c ) ) ) 

rm( drop_id, fl_iso2c , retain_id )

pop_raw__country_colrename <-pop_raw_country_filter %>% transmute( country = country,
                         population=SP.POP.TOTL )

pop <- pop_raw__country_colrename

rm(pop_raw__country_colrename,pop_raw_country_filter)

#######################################################33

#Merge the two tables
df <- full_join(cv,pop)

# Correct some country names by hand
use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic","Korea, Rep.","Kyrgyz Republic",
              "Laos","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines",
              "Slovak Republic","United States","Myanmar")

alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia","Korea, South","Kyrgyzstan",
                "Lao PDR","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
                "Slovakia","US","Burma")

# Simply use a for-cycle to change the name for the countries (note: ordering is important)
for ( i in seq_along( use_name ) ){
  df$country[ df$country == alter_name[ i ] ] <- use_name[ i ]
}

# Write a for-loop to find those which are partial or complete matches!
# 1) auxillary table for countries without any population value
aux <- df %>% filter( is.na(population) )
# 2) Get the name of the countries
countries_nm <- aux$country
# 3) Iterate through all potential partial matches
for ( i in seq_along( countries_nm ) ){
  # Select those observations where partial match exists
  log_select <- str_detect( df$country , countries_nm[ i ] )
  # Get the population values for partial matches
  c_partial <- df$population[ log_select ]
  # If there is a match: only two countries are selected and one is missing the other has population:
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # Replace the missing value with the match
    df$population[ log_select & is.na(df$population)] = c_partial[ !is.na( c_partial ) ]
    # Remove the replaced variable
    df <- df %>% filter( !(log_select & is.na( df$confirmed ) ) )
  }
}

# 4) Check the results:
df %>% filter( is.na(population) )
# These are:
#   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by world bank 
#       (Western Sahara, Taiwan or Kosovo)
#   c) we have no population data on them (Ertirea, Holy See (Vatican))

#####
# Handle missing values:
View( df %>% filter( !complete.cases(df) ) )
# Drop if population, confirmed cases or death is missing
df <- df %>% filter( !( is.na( population ) | is.na( confirmed ) | is.na( death ) ))


#####
# Save clean data

# COVID data
write_csv( df , paste0(my_path,'clean/covid_pop_09_24_2020_clean.csv'))




 

                    