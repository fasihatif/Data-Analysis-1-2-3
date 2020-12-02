# Packages
library(tidyverse)
library(ggthemes)

# df <- read_csv(paste0(dir,"/clean/hotels-europe_clean.csv"))
my_url_clean <- "https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/atif-fasih_da2_data-exercise/CH7_Q3/data/clean/hotels-europe_clean.csv"
df <- read_csv( my_url_clean )

df_ams_filter <- df %>% filter(city_actual == "Amsterdam" &
                                accommodation_type == "Hotel" &
                                weekend == 0 & 
                                year == 2017 & 
                                month == 11 & 
                                stars %in% c(3.0,3.5,4.0))

# Filter for relevant columns
df_ams <- df_ams_filter %>% select(city_actual,price,distance,stars)


# Examining the distribution of the distance variable
df_ams %>%  ggplot(aes(distance)) + 
  geom_histogram(color = 'black') + 
  theme_wsj() + scale_fill_wsj() + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6))

df_ams <- df_ams %>% mutate(dist_category = ifelse(distance < 2,"close","far"))
df_ams <- df_ams %>% mutate(dist_category = factor(dist_category))
                          
# Binscatter, 2bins
df_ams %>% group_by(dist_category) %>%
  summarise("mean_price" = round(mean(price))) %>%
  ggplot(aes(dist_category, mean_price, label = mean_price)) + geom_point() +
  labs(x = " Distance to city center (categories)", y = "Average Price ($)")+ coord_cartesian(ylim = c(50, 200)) + geom_text(vjust = -1)

# Binscatter, 4 bins
ggplot(df_ams, aes(x=distance,y=price)) + 
  stat_summary_bin(fun='mean', bins = 4, color='red', size=2, geom='point',breaks = c(0,1,2,3,6)) + coord_cartesian(ylim = c(50, 200), xlim = c(0,6)) + labs(x = " Distance to city center (miles)", y = "Price ($)") +scale_x_continuous(breaks = c(1,2,3,4,5,6))

# Loess Regression
ggplot(df_ams, aes(x=distance,y=price)) + geom_point() + geom_smooth(method = "loess") + coord_cartesian(ylim = c(50, 350)) + labs(x = " Distance to city center (miles)", y = "Price ($)") +  scale_x_continuous(breaks = c(1,2,3,4,5,6))


# Linear Regression

df_ams_reg <- lm(price ~ distance, data = df_ams)
summary(df_ams_reg)

ggplot( data = df_ams, aes( x = distance, y = price) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' ) +
  ggtitle("Linear Regression") +
  labs(x = "Distance to city center (Miles)",y = "Price($)")

